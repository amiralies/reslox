exception EvalError(string, Location.t)

exception Return(Value.t)

open Ast

open! Value

let env = {
  let env = ref(Env.empty)
  Globals.globals->List.forEach(((name, value)) => env := Env.define(env.contents, name, value))
  env
}

let rec findMethod = (class, methodName) =>
  switch (Map.String.get(class.methods, methodName), class.maybeSuperClass) {
  | (Some(method), _) => Some(method)
  | (None, Some(superclass)) => findMethod(superclass, methodName)
  | (None, None) => None
  }

let isTruthy = value =>
  switch value {
  | VNil | VBool(false) => false
  | _ => true
  }

let applyArthimaticOrRaise = (opLoc, left, right, f) =>
  switch (left, right) {
  | (VNumber(left), VNumber(right)) => VNumber(f(left, right))
  | _ => raise(EvalError("Operands must be numbers.", opLoc))
  }

let applyComparisonOrRaise = (opLoc, left, right, p) =>
  switch (left, right) {
  | (VNumber(left), VNumber(right)) => VBool(p(left, right))
  | _ => raise(EvalError("Operands must be numbers.", opLoc))
  }

let rec evaluate = expr =>
  switch expr.exprDesc {
  | ExprBinary(left, op, right) =>
    let leftValue = evaluate(left)
    let rightValue = evaluate(right)
    evalBinary(leftValue, op, rightValue)
  | ExprGrouping(expr) => evaluate(expr)
  | ExprLiteral(value) => value
  | ExprUnary(op, right) =>
    let rightValue = evaluate(right)

    evalUnary(op, rightValue)
  | ExprConditional(cond, thenBranch, elseBranch) =>
    let condValue = evaluate(cond)

    evalConditional(condValue, thenBranch, elseBranch)
  | ExprVariable(name) =>
    switch Env.get(env.contents, name) {
    | Some(value) => value
    | None => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprAssign(name, expr) =>
    let value = evaluate(expr)
    switch Env.assign(env.contents, name, value) {
    | Ok(newEnv) =>
      env := newEnv
      value
    | Error() => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprLogical(left, op, right) =>
    switch op.lopDesc {
    | LopOr =>
      let leftValue = evaluate(left)
      if isTruthy(leftValue) {
        leftValue
      } else {
        evaluate(right)
      }
    | LopAnd =>
      let leftValue = evaluate(left)
      if isTruthy(leftValue) {
        evaluate(right)
      } else {
        leftValue
      }
    }

  | ExprCall(callee, arguments) =>
    let calleeValue = evaluate(callee)

    let argumentsValues = arguments->List.map(argument => evaluate(argument))

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

    | VClass(class) =>
      switch findMethod(class, "init") {
      | None =>
        if List.length(argumentsValues) > 0 {
          raise(
            EvalError(
              "Expected 0 arguments but got " ++ List.length(arguments)->Int.toString ++ ".",
              callee.exprLoc,
            ),
          )
        }
        let instance = {class: class, fields: MutableMap.String.make()}
        VInstance(instance)

      | Some(init) =>
        let instance = {class: class, fields: MutableMap.String.make()}
        let initCallable = init.bind(instance)

        if initCallable.arity != List.length(argumentsValues) {
          raise(
            EvalError(
              "Expected " ++
              initCallable.arity->Int.toString ++
              " arguments but got " ++
              List.length(arguments)->Int.toString ++ ".",
              callee.exprLoc,
            ),
          )
        }

        let _: Value.t = initCallable.call(argumentsValues)
        VInstance(instance)
      }

    | VNil | VString(_) | VNumber(_) | VBool(_) | VInstance(_) =>
      raise(EvalError("Can only call functions and classes.", callee.exprLoc))
    }
  | ExprGet(object, propName) =>
    let maybeObject = evaluate(object)

    let instance = switch maybeObject {
    | VInstance(instance) => instance
    | _ => raise(EvalError("Only instances have properties.", object.exprLoc))
    }

    switch MutableMap.String.get(instance.fields, propName) {
    | Some(field) => field

    | None =>
      switch findMethod(instance.class, propName) {
      | Some(method) => VFunction(method.bind(instance))
      | None => raise(EvalError(`Undefined property '${propName}'.`, object.exprLoc))
      }
    }

  | ExprSet(object, propName, value) =>
    let maybeObject = evaluate(object)

    let instance = switch maybeObject {
    | VInstance(instance) => instance
    | _ => raise(EvalError("Only instances have fields.", object.exprLoc))
    }

    let value = evaluate(value)
    instance.fields->MutableMap.String.set(propName, value)
    value
  | ExprThis =>
    switch Env.get(env.contents, "this") {
    | Some(value) => value
    | None => raise(EvalError("Cann't use this outside of a method", expr.exprLoc)) // TODO
    }

  | ExprSuper(methodName) =>
    switch Env.get(env.contents, "super") {
    | Some(VClass(superclass)) =>
      switch superclass->findMethod(methodName) {
      | None => raise(EvalError(`Undefined property '${methodName}'.`, expr.exprLoc)) // TODO
      | Some(method) =>
        let this = switch Env.get(env.contents, "this") {
        | Some(VInstance(instance)) => instance
        | None | Some(_) => raise(Failure("Internal error"))
        }

        VFunction(method.bind(this))
      }
    | Some(_) => raise(Failure("Internal error super cannot be anything other VClass"))
    | None => raise(EvalError("no super allowed here or class is not folan", expr.exprLoc)) // TODO
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
    | _ => raise(EvalError("Operands must be two numbers or two strings.", bopLoc))
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
      raise(EvalError("Operand must be a number.", uopLoc))
    }

  | UopNot => VBool(!isTruthy(right))
  }

and evalConditional = (cond, thenBranch, elseBranch) =>
  if isTruthy(cond) {
    evaluate(thenBranch)
  } else {
    evaluate(elseBranch)
  }

let rec execute = (stmt: Ast.stmt) =>
  switch stmt.stmtDesc {
  | StmtExpression(expr) =>
    let _: Value.t = evaluate(expr)

  | StmtPrint(expr) =>
    let value = evaluate(expr)
    Js.log(Value.printValue(value))

  | StmtVar(name, maybeInitExpr) =>
    let value = switch maybeInitExpr {
    | Some(initExpr) => evaluate(initExpr)
    | None => VNil
    }

    let newEnv = Env.define(env.contents, name, value)
    env := newEnv

  | StmtBlock(statements) => executeBlock(statements)

  | StmtIf(condition, thenBranch, elseBranch) =>
    let conditionValue = evaluate(condition)

    if isTruthy(conditionValue) {
      execute(thenBranch)
    } else {
      Option.forEach(elseBranch, execute)
    }

  | StmtWhile(condition, body) =>
    while isTruthy(evaluate(condition)) {
      execute(body)
    }
  | StmtFunction(name, parameters, body) =>
    let closure = ref(env.contents)
    let call = arguments => {
      let currentEnv = env.contents
      env := Env.enterBlock(closure.contents)
      parameters->List.forEachWithIndex((i, parameter) =>
        env := Env.define(env.contents, parameter, arguments->List.getExn(i))
      )

      let value = try {
        executeBlock(body)
        VNil
      } catch {
      | Return(value) => value
      }

      closure := Env.exitBlock(env.contents)
      env := currentEnv
      value
    }
    let callable = VFunction({
      toString: `<fn ${name}>`,
      name: name,
      arity: parameters->List.length,
      call: call,
    })

    env := Env.define(env.contents, name, callable)
    closure := Env.define(closure.contents, name, callable)
  | StmtReturn(maybeExpr) =>
    let value = maybeExpr->Option.mapWithDefault(VNil, evaluate)

    raise(Return(value))
  | StmtClass(name, maybeSuperClassName, methodDecls) =>
    // TODO Refactor

    let maybeSuperClassValue =
      maybeSuperClassName->Option.flatMap(superClassName => Env.get(env.contents, superClassName))

    let maybeSuperClass = switch maybeSuperClassValue {
    | None => None
    | Some(VClass(class)) => Some(class)
    | Some(_) => raise(EvalError("Superclass must be a class.", stmt.stmtLoc))
    }

    switch maybeSuperClass {
    | None => ()
    | Some(superClass) => env := Env.define(env.contents, "super", VClass(superClass))
    }

    let methods = methodDecls->List.map(method => {
      let closure = ref(env.contents)
      let isInit = method.name == "init"
      let bind = instance => {
        let call = arguments => {
          let currentEnv = env.contents
          env := Env.enterBlock(closure.contents)
          env := Env.define(env.contents, "this", VInstance(instance))

          method.parameters->List.forEachWithIndex((i, parameter) =>
            env := Env.define(env.contents, parameter, arguments->List.getExn(i))
          )

          let value = try {
            executeBlock(method.body)
            VNil
          } catch {
          | Return(value) => value
          }

          let value = isInit ? VInstance(instance) : value

          closure := Env.exitBlock(env.contents)
          env := currentEnv
          value
        }
        {
          name: method.name,
          call: call,
          arity: method.parameters->List.length,
          toString: `<fn ${method.name}>`,
        }
      }

      ({name: method.name, bind: bind}, closure)
    })

    let closures = List.map(methods, snd)
    let methods =
      List.map(methods, fst)
      ->List.map(method => (method.name, method))
      ->List.toArray
      ->Map.String.fromArray

    let class = VClass({name: name, maybeSuperClass: maybeSuperClass, methods: methods})

    closures->List.forEach(closure => {
      closure := Env.define(closure.contents, name, class)
    })

    env := Env.define(env.contents, name, class)
  }
and executeBlock = statements => {
  env := Env.enterBlock(env.contents)
  List.forEach(statements, execute)
  env := Env.exitBlock(env.contents)
}

let interpret = (program: list<Ast.stmt>) => {
  switch List.forEach(program, execute) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }
}
