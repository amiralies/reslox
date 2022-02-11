exception EvalError(string, Location.t)

open Ast

let isTruthy = value =>
  switch value {
  | VNil => false
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

let rec evaluate: Ast.expr => Ast.value = expr =>
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
  | ExprConditional(cond, then, else_) =>
    let condValue = evaluate(cond)

    evalConditional(condValue, then, else_)
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

and evalConditional = (cond, then, else_) =>
  if isTruthy(cond) {
    evaluate(then)
  } else {
    evaluate(else_)
  }

let execute = (stmt: Ast.stmt) =>
  switch stmt.stmtDesc {
  | StmtExpression(expr) =>
    let _: Ast.value = evaluate(expr)

  | StmtPrint(expr) =>
    let value = evaluate(expr)
    Js.log(Ast.printValue(value))
  | _ => ()
  }

let interpret = (program: list<Ast.stmt>) =>
  switch List.forEach(program, execute) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }

