exception EvalError(string, Location.t)

open Ast

type value = Value.t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil

let isTruthy = value =>
  switch value {
  | Value.VNil => false
  | VBool(b) => b
  | VNumber(_) | VString(_) => true
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

let rec evaluate: (Env.t, Ast.expr) => value = (env, expr) =>
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
  | ExprConditional(cond, then, else_) =>
    let condValue = evaluate(env, cond)

    evalConditional(env, condValue, then, else_)
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
  | BopCommaSeq => right
  }

and evalUnary = ({uopDesc, uopLoc}, right) =>
  switch uopDesc {
  | UopNegative =>
    switch right {
    | VNumber(number) => VNumber(-.number)
    | VString(_) | VBool(_) | VNil =>
      raise(EvalError("Can't use unary '-' operator on non-number values", uopLoc))
    }

  | UopNot => VBool(!isTruthy(right))
  }

and evalConditional = (env, cond, then, else_) =>
  if isTruthy(cond) {
    evaluate(env, then)
  } else {
    evaluate(env, else_)
  }

let execute = (env: Env.t, stmt: Ast.stmt) =>
  switch stmt.stmtDesc {
  | StmtExpression(expr) =>
    let _: Value.t = evaluate(env, expr)

  | StmtPrint(expr) =>
    let value = evaluate(env, expr)
    Js.log(Value.print(value))

  | StmtVar(name, initExpr) =>
    let value = evaluate(env, initExpr)

    Env.define(env, name, value)
  }

let interpret = (program: list<Ast.stmt>) => {
  let env = Env.make()

  switch List.forEach(program, execute(env)) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }
}

