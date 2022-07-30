exception EvalError(string, Location.t)

exception Return(Value.t)

open Ast

open! Value

type envContainer = {mutable env: Env.t<Value.t>}

let makeEnvContainer = () => {
  env: Env.empty,
}

let isTruthy = value =>
  switch value {
  | VNil | VBool(false) => false
  | _ => true
  }

let applyArthimaticOrRaise = (opLoc, left, right, f) =>
  switch (left, right) {
  | (VNumber(left), VNumber(right)) => VNumber(f(left, right))
  | _ => raise(EvalError("Operands should be numbers", opLoc))
  }

let applyComparisonOrRaise = (opLoc, left, right, p) =>
  switch (left, right) {
  | (VNumber(left), VNumber(right)) => VBool(p(left, right))
  | _ => raise(EvalError("Operands should be numbers", opLoc))
  }

let rec evaluate = (envContainer: envContainer, expr) =>
  switch expr.exprDesc {
  | ExprBinary(left, op, right) =>
    let leftValue = evaluate(envContainer, left)
    let rightValue = evaluate(envContainer, right)
    evalBinary(leftValue, op, rightValue)
  | ExprGrouping(expr) => evaluate(envContainer, expr)
  | ExprLiteral(value) => value
  | ExprUnary(op, right) =>
    let rightValue = evaluate(envContainer, right)

    evalUnary(op, rightValue)
  | ExprConditional(cond, thenBranch, elseBranch) =>
    let condValue = evaluate(envContainer, cond)

    evalConditional(envContainer, condValue, thenBranch, elseBranch)
  | ExprVariable(name) =>
    switch Env.get(envContainer.env, name) {
    | Some(value) => value
    | None => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprAssign(name, expr) =>
    let value = evaluate(envContainer, expr)
    switch Env.assign(envContainer.env, name, value) {
    | Ok(newEnv) =>
      envContainer.env = newEnv
      value
    | Error() => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprLogical(left, op, right) =>
    switch op.lopDesc {
    | LopOr =>
      let leftValue = evaluate(envContainer, left)
      if isTruthy(leftValue) {
        leftValue
      } else {
        evaluate(envContainer, right)
      }
    | LopAnd =>
      let leftValue = evaluate(envContainer, left)
      if isTruthy(leftValue) {
        evaluate(envContainer, right)
      } else {
        leftValue
      }
    }

  | ExprCall(callee, arguments) =>
    let calleeValue = evaluate(envContainer, callee)

    let argumentsValues = arguments->List.map(argument => evaluate(envContainer, argument))

    switch calleeValue {
    | VFunction(callable) =>
      if callable.arity == List.length(argumentsValues) {
        callable.call(argumentsValues)
      } else {
        raise(
          EvalError(
            "Expected " ++
            callable.arity->Int.toString ++
            " arguments but got " ++
            List.length(arguments)->Int.toString ++ ".",
            callee.exprLoc,
          ),
        )
      }

    | VClass(class) => VInstance({class: class, fields: MutableMap.String.make()})
    | VNil | VString(_) | VNumber(_) | VBool(_) | VInstance(_) =>
      raise(EvalError("Can only call functions and classes.", callee.exprLoc))
    }
  | ExprGet(object, propName) =>
    let maybeObject = evaluate(envContainer, object)

    let instance = switch maybeObject {
    | VInstance(instance) => instance
    | _ => raise(EvalError("Only instances have properties", object.exprLoc))
    }

    switch MutableMap.String.get(instance.fields, propName) {
    | Some(field) => field

    | None =>
      switch Map.String.get(instance.class.methods, propName) {
      | Some(method) => VFunction(method.invoke(instance))
      | None => raise(EvalError(`Undefined property '${propName}'.`, object.exprLoc))
      }
    }

  | ExprSet(object, propName, value) =>
    let maybeObject = evaluate(envContainer, object)

    let instance = switch maybeObject {
    | VInstance(instance) => instance
    | _ => raise(EvalError("Only instances have fields", object.exprLoc))
    }

    let value = evaluate(envContainer, value)
    instance.fields->MutableMap.String.set(propName, value)
    value
  | ExprThis =>
    switch Env.get(envContainer.env, "this") {
    | Some(value) => value
    | None => raise(EvalError("Cann't use this outside of a method", expr.exprLoc)) // TODO
    }
  }

and evalBinary = (left, {bopDesc, bopLoc}, right) =>
  switch bopDesc {
  | BopSub => applyArthimaticOrRaise(bopLoc, left, right, (l, r) => l -. r)
  | BopDiv => applyArthimaticOrRaise(bopLoc, left, right, (l, r) => l /. r) // TODO Division by zero
  | BopMul => applyArthimaticOrRaise(bopLoc, left, right, (l, r) => l *. r)
  | BopAdd =>
    switch (left, right) {
    | (VNumber(left), VNumber(right)) => VNumber(left +. right)
    | (VString(left), VString(right)) => VString(left ++ right)
    | _ => raise(EvalError("Both operands should be numbers or strings", bopLoc))
    }
  | BopGreater => applyComparisonOrRaise(bopLoc, left, right, (l, r) => l > r)
  | BopGreaterEqual => applyComparisonOrRaise(bopLoc, left, right, (l, r) => l >= r)
  | BopLess => applyComparisonOrRaise(bopLoc, left, right, (l, r) => l < r)
  | BopLessEqual => applyComparisonOrRaise(bopLoc, left, right, (l, r) => l <= r)
  | BopEqual => VBool(left == right)
  | BopNotEqual => VBool(left != right)
  }

and evalUnary = ({uopDesc, uopLoc}, right) =>
  switch uopDesc {
  | UopNegative =>
    switch right {
    | VNumber(number) => VNumber(-.number)
    | VInstance(_) | VClass(_) | VFunction(_) | VString(_) | VBool(_) | VNil =>
      raise(EvalError("Can't use unary '-' operator on non-number values", uopLoc))
    }

  | UopNot => VBool(!isTruthy(right))
  }

and evalConditional = (envContainer: envContainer, cond, thenBranch, elseBranch) =>
  if isTruthy(cond) {
    evaluate(envContainer, thenBranch)
  } else {
    evaluate(envContainer, elseBranch)
  }

let rec execute = (envContainer: envContainer, stmt: Ast.stmt) =>
  switch stmt.stmtDesc {
  | StmtExpression(expr) =>
    let _: Value.t = evaluate(envContainer, expr)

  | StmtPrint(expr) =>
    let value = evaluate(envContainer, expr)
    Js.log(Value.printValue(value))

  | StmtVar(name, maybeInitExpr) =>
    let value = switch maybeInitExpr {
    | Some(initExpr) => evaluate(envContainer, initExpr)
    | None => VNil
    }

    let newEnv = Env.define(envContainer.env, name, value)
    envContainer.env = newEnv

  | StmtBlock(statements) => executeBlock(envContainer, statements)

  | StmtIf(condition, thenBranch, elseBranch) =>
    let conditionValue = evaluate(envContainer, condition)

    if isTruthy(conditionValue) {
      execute(envContainer, thenBranch)
    } else {
      Option.forEach(elseBranch, execute(envContainer, _))
    }

  | StmtWhile(condition, body) =>
    let conditionValue = evaluate(envContainer, condition)
    if isTruthy(conditionValue) {
      execute(envContainer, body)
      execute(envContainer, stmt)
    }
  | StmtFunction(name, parameters, body) =>
    let callable = {
      let closureEnvContainer = {env: envContainer.env}
      let callable = VFunction({
        name: name,
        arity: parameters->List.length,
        call: arguments => {
          let closure = {env: closureEnvContainer.env}
          parameters->List.forEachWithIndex((i, parameter) =>
            closure.env = Env.define(closure.env, parameter, arguments->List.getExn(i))
          )

          let value = try {
            executeBlock(closure, body)
            VNil
          } catch {
          | Return(value) => value
          }

          value
        },
      })
      closureEnvContainer.env = Env.define(closureEnvContainer.env, name, callable)
      callable
    }

    envContainer.env = Env.define(envContainer.env, name, callable)
  | StmtReturn(maybeExpr) =>
    let value = maybeExpr->Option.mapWithDefault(VNil, evaluate(envContainer, _))

    raise(Return(value))
  | StmtClass(name, methodDecls) =>
    // TODO Refactor
    let methods = methodDecls->List.map(method => {
      let closureEnvContainer = {env: envContainer.env}
      let invoke = instance => {
        let call = arguments => {
          let closure = {env: closureEnvContainer.env}
          closure.env = Env.define(closure.env, "this", VInstance(instance))

          method.parameters->List.forEachWithIndex((i, parameter) =>
            closure.env = Env.define(closure.env, parameter, arguments->List.getExn(i))
          )

          let value = try {
            executeBlock(closure, method.body)
            VNil
          } catch {
          | Return(value) => value
          }

          value
        }
        {name: method.name, call: call, arity: method.parameters->List.length}
      }

      ({name: method.name, invoke: invoke}, closureEnvContainer)
    })

    let closures = List.map(methods, snd)
    let methods =
      List.map(methods, fst)
      ->List.map(method => (method.name, method))
      ->List.toArray
      ->Map.String.fromArray

    let class = VClass({name: name, methods: methods})

    closures->List.forEach(closure => {
      closure.env = Env.define(closure.env, name, class)
    })

    envContainer.env = Env.define(envContainer.env, name, class)
  }
and executeBlock = (envContainer: envContainer, statements) => {
  envContainer.env = Env.enterBlock(envContainer.env)
  List.forEach(statements, execute(envContainer))
  envContainer.env = Env.exitBlock(envContainer.env)
}

let interpret = (program: list<Ast.stmt>) => {
  let envContainer = makeEnvContainer()
  Globals.globals->List.forEach(((name, value)) =>
    envContainer.env = Env.define(envContainer.env, name, value)
  )

  switch List.forEach(program, execute(envContainer)) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }
}
