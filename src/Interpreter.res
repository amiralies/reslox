exception EvalError(string, Location.t)

exception Return(Value.t)

open Ast

open Value

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

let rec evaluate: (Env.t<'a>, Ast.expr) => Value.t = (env, expr) =>
  switch expr.exprDesc {
  | ExprBinary(left, op, right) =>
    let leftValue = evaluate(env, left)
    let rightValue = evaluate(env, right)
    evalBinary(leftValue, op, rightValue)
  | ExprGrouping(expr) => evaluate(env, expr)
  | ExprLiteral(value) => value
  | ExprUnary(op, right) =>
    let rightValue = evaluate(env, right)

    evalUnary(op, rightValue)
  | ExprConditional(cond, thenBranch, elseBranch) =>
    let condValue = evaluate(env, cond)

    evalConditional(env, condValue, thenBranch, elseBranch)
  | ExprVariable(name) =>
    switch Env.get(env, name) {
    | Some(value) => value
    | None => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprAssign(name, expr) =>
    let value = evaluate(env, expr)
    switch Env.assign(env, name, value) {
    | Ok() => value
    | Error() => raise(EvalError("Undefined variable '" ++ name ++ "'.", expr.exprLoc))
    }
  | ExprLogical(left, op, right) =>
    switch op.lopDesc {
    | LopOr =>
      let leftValue = evaluate(env, left)
      if isTruthy(leftValue) {
        leftValue
      } else {
        evaluate(env, right)
      }
    | LopAnd =>
      let leftValue = evaluate(env, left)
      if isTruthy(leftValue) {
        evaluate(env, right)
      } else {
        leftValue
      }
    }

  | ExprCall(callee, arguments) =>
    let calleeValue = evaluate(env, callee)

    let argumentsValues = arguments->List.map(argument => evaluate(env, argument))

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

let rec execute = (env: Env.t<'a>, stmt: Ast.stmt) =>
  switch stmt.stmtDesc {
  | StmtExpression(expr) =>
    let _: Value.t = evaluate(env, expr)

  | StmtPrint(expr) =>
    let value = evaluate(env, expr)
    Js.log(Value.printValue(value))

  | StmtVar(name, initExpr) =>
    let value = evaluate(env, initExpr)

    Env.define(env, name, value)

  | StmtBlock(statements) =>
    let newEnv = Env.make(~enclosing=env, ())
    executeBlock(newEnv, statements)

  | StmtIf(condition, thenBranch, elseBranch) =>
    let conditionValue = evaluate(env, condition)

    if isTruthy(conditionValue) {
      execute(env, thenBranch)
    } else {
      Option.forEach(elseBranch, execute(env, _))
    }

  | StmtWhile(condition, body) =>
    let conditionValue = evaluate(env, condition)
    if isTruthy(conditionValue) {
      execute(env, body)
      execute(env, stmt)
    }
  | StmtFunction(name, parameters, body) =>
    let callable = VCallable({
      toString: "<fn " ++ name ++ ">",
      arity: parameters->List.length,
      closure: env,
      call: (closure, arguments) => {
        let env = Env.make(~enclosing=closure, ())
        parameters->List.forEachWithIndex((i, parameter) =>
          Env.define(env, parameter, arguments->List.getExn(i))
        )

        let value = try {
          executeBlock(env, body)
          VNil
        } catch {
        | Return(value) => value
        }

        value
      },
    })

    Env.define(env, name, callable)
  | StmtReturn(maybeExpr) =>
    let value = maybeExpr->Option.mapWithDefault(VNil, evaluate(env, _))

    raise(Return(value))
  }
and executeBlock = (env, statements) => {
  List.forEach(statements, execute(env))
}

let interpret = (program: list<Ast.stmt>) => {
  let env = global

  switch List.forEach(program, execute(env)) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }
}
