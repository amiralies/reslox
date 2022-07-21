exception EvalError(string, Location.t)

exception Return(Value.t)

open Ast

open Value

type t = {mutable env: Env.t<Value.t>}

let global = {
  let globals = Env.empty
  globals->Env.define(
    "clock",
    VFunction({
      arity: 0,
      toString: "<native fn>",
      call: _ => VNumber((Js.Date.now() /. 1000.0)->Js.Math.floor_float),
    }),
  )
}

let make = () => {
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

let rec evaluate = (interpreter: t, expr) =>
  switch expr.exprDesc {
  | ExprBinary(left, op, right) =>
    let leftValue = evaluate(interpreter, left)
    let rightValue = evaluate(interpreter, right)
    evalBinary(leftValue, op, rightValue)
  | ExprGrouping(expr) => evaluate(interpreter, expr)
  | ExprLiteral(value) => value
  | ExprUnary(op, right) =>
    let rightValue = evaluate(interpreter, right)

    evalUnary(op, rightValue)
  | ExprConditional(cond, thenBranch, elseBranch) =>
    let condValue = evaluate(interpreter, cond)

    evalConditional(interpreter, condValue, thenBranch, elseBranch)
  | ExprVariable(name) =>
    switch Env.get(interpreter.env, name) {
    | Some(value) => value
    | None => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprAssign(name, expr) =>
    let value = evaluate(interpreter, expr)
    switch Env.assign(interpreter.env, name, value) {
    | Ok(newEnv) =>
      interpreter.env = newEnv
      value
    | Error() => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprLogical(left, op, right) =>
    switch op.lopDesc {
    | LopOr =>
      let leftValue = evaluate(interpreter, left)
      if isTruthy(leftValue) {
        leftValue
      } else {
        evaluate(interpreter, right)
      }
    | LopAnd =>
      let leftValue = evaluate(interpreter, left)
      if isTruthy(leftValue) {
        evaluate(interpreter, right)
      } else {
        leftValue
      }
    }

  | ExprCall(callee, arguments) =>
    let calleeValue = evaluate(interpreter, callee)

    let argumentsValues = arguments->List.map(argument => evaluate(interpreter, argument))

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
    | _ => raise(EvalError("Can only call functions and classes.", callee.exprLoc))
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
    | VFunction(_) | VString(_) | VBool(_) | VNil =>
      raise(EvalError("Can't use unary '-' operator on non-number values", uopLoc))
    }

  | UopNot => VBool(!isTruthy(right))
  }

and evalConditional = (interpreter: t, cond, thenBranch, elseBranch) =>
  if isTruthy(cond) {
    evaluate(interpreter, thenBranch)
  } else {
    evaluate(interpreter, elseBranch)
  }

let rec execute = (interpreter: t, stmt: Ast.stmt) =>
  switch stmt.stmtDesc {
  | StmtExpression(expr) =>
    let _: Value.t = evaluate(interpreter, expr)

  | StmtPrint(expr) =>
    let value = evaluate(interpreter, expr)
    Js.log(Value.printValue(value))

  | StmtVar(name, maybeInitExpr) =>
    let value = switch maybeInitExpr {
    | Some(initExpr) => evaluate(interpreter, initExpr)
    | None => VNil
    }

    let newEnv = Env.define(interpreter.env, name, value)
    interpreter.env = newEnv

  | StmtBlock(statements) => executeBlock(interpreter, statements)

  | StmtIf(condition, thenBranch, elseBranch) =>
    let conditionValue = evaluate(interpreter, condition)

    if isTruthy(conditionValue) {
      execute(interpreter, thenBranch)
    } else {
      Option.forEach(elseBranch, execute(interpreter, _))
    }

  | StmtWhile(condition, body) =>
    let conditionValue = evaluate(interpreter, condition)
    if isTruthy(conditionValue) {
      execute(interpreter, body)
      execute(interpreter, stmt)
    }
  | StmtFunction(name, parameters, body) =>
    let callable = {
      let closure = {env: interpreter.env}
      let callable = VFunction({
        toString: "<fn " ++ name ++ ">",
        arity: parameters->List.length,
        call: arguments => {
          let closure = {env: closure.env}
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
      closure.env = Env.define(closure.env, name, callable)
      callable
    }

    interpreter.env = Env.define(interpreter.env, name, callable)
  | StmtReturn(maybeExpr) =>
    let value = maybeExpr->Option.mapWithDefault(VNil, evaluate(interpreter, _))

    raise(Return(value))
  }
and executeBlock = (interpreter: t, statements) => {
  interpreter.env = Env.enterBlock(interpreter.env)
  List.forEach(statements, execute(interpreter))
  interpreter.env = Env.exitBlock(interpreter.env)
}

let interpret = (program: list<Ast.stmt>) => {
  let interpreter = make()
  interpreter.env = global

  switch List.forEach(program, execute(interpreter)) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }
}
