exception EvalError(string, Location.t)

exception Return(Value.t)

open Ast

open Value

type t = {mutable env: Env.t<Value.t>}

let global = {
  let globals = Env.make()
  globals->Env.define(
    "clock",
    VCallable({
      toString: "<native fn>",
      arity: 0,
      closure: Env.make(),
      call: (_, _) => VNumber((Js.Date.now() /. 1000.0)->Js.Math.floor_float),
    }),
  )
  globals
}

let make = () => {
  env: Env.make(),
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

let rec evaluate = (interpreter, expr) =>
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
    | Ok() => value
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
    | VCallable(callable) =>
      if callable.arity == List.length(argumentsValues) {
        callable.call(callable.closure, argumentsValues)
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
    | VCallable(_) | VString(_) | VBool(_) | VNil =>
      raise(EvalError("Can't use unary '-' operator on non-number values", uopLoc))
    }

  | UopNot => VBool(!isTruthy(right))
  }

and evalConditional = (env, cond, thenBranch, elseBranch) =>
  if isTruthy(cond) {
    evaluate(env, thenBranch)
  } else {
    evaluate(env, elseBranch)
  }

let rec execute = (interpreter, stmt: Ast.stmt) =>
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

    Env.define(interpreter.env, name, value)

  | StmtBlock(statements) =>
    interpreter.env = Env.make(~enclosing=interpreter.env, ())
    executeBlock(interpreter, statements)

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
    let callable = VCallable({
      toString: "<fn " ++ name ++ ">",
      arity: parameters->List.length,
      closure: interpreter.env,
      call: (closure, arguments) => {
        let env = Env.make(~enclosing=closure, ())
        parameters->List.forEachWithIndex((i, parameter) =>
          Env.define(env, parameter, arguments->List.getExn(i))
        )

        let value = try {
          executeBlock(interpreter, body)
          VNil
        } catch {
        | Return(value) => value
        }

        value
      },
    })

    Env.define(interpreter.env, name, callable)
  | StmtReturn(maybeExpr) =>
    let value = maybeExpr->Option.mapWithDefault(VNil, evaluate(interpreter, _))

    raise(Return(value))
  }
and executeBlock = (interpreter: t, statements) => {
  List.forEach(statements, execute(interpreter))
}

let interpret = (program: list<Ast.stmt>) => {
  let interpreter = make()
  interpreter.env = global

  switch List.forEach(program, execute(interpreter)) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }
}
