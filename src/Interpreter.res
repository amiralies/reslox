exception EvalError(string, Location.t)

open Expr

let isTruthy = value =>
  switch value {
  | Expr.Nil => false
  | Bool(b) => b
  | Number(_) | String(_) => true
  }

let applyArthimaticOrRaise = (opLoc, left, right, f) =>
  switch (left, right) {
  | (Number(left), Number(right)) => Number(f(left, right))
  | _ => raise(EvalError("Operands should be numbers", opLoc))
  }

let applyComparisonOrRaise = (opLoc, left, right, p) =>
  switch (left, right) {
  | (Number(left), Number(right)) => Bool(p(left, right))
  | _ => raise(EvalError("Operands should be numbers", opLoc))
  }

let rec evaluate: Expr.t => Expr.value = expr =>
  switch expr.val {
  | Binary(left, op, right) =>
    let leftValue = evaluate(left)
    let rightValue = evaluate(right)
    evalBinary(leftValue, op, rightValue)
  | Grouping(expr) => evaluate(expr)
  | Literal(value) => value.val
  | Unary(op, right) =>
    let rightValue = evaluate(right)

    evalUnary(op, rightValue)
  | Conditional(cond, then, else_) =>
    let condValue = evaluate(cond)

    evalConditional(condValue, then, else_)
  }

and evalBinary = (left, {val: op, loc: opLoc}, right) =>
  switch op {
  | Sub => applyArthimaticOrRaise(opLoc, left, right, (l, r) => l -. r)
  | Div => applyArthimaticOrRaise(opLoc, left, right, (l, r) => l /. r) // TODO Division by zero
  | Mul => applyArthimaticOrRaise(opLoc, left, right, (l, r) => l *. r)
  | Add =>
    switch (left, right) {
    | (Number(left), Number(right)) => Number(left +. right)
    | (String(left), String(right)) => String(left ++ right)
    | _ => raise(EvalError("Both operands should be numbers or strings", opLoc))
    }
  | GreaterThan => applyComparisonOrRaise(opLoc, left, right, (l, r) => l > r)
  | GreaterThanEqual => applyComparisonOrRaise(opLoc, left, right, (l, r) => l >= r)
  | LessThan => applyComparisonOrRaise(opLoc, left, right, (l, r) => l < r)
  | LessThanEqual => applyComparisonOrRaise(opLoc, left, right, (l, r) => l <= r)
  | Equal => Bool(left == right)
  | NotEqual => Bool(left != right)
  | CommaSequence => right
  }

and evalUnary = ({val: op, loc: opLoc}, right) =>
  switch op {
  | Negative =>
    switch right {
    | Number(number) => Number(-.number)
    | String(_) | Bool(_) | Nil =>
      raise(EvalError("Can't use unary '-' operator on non-number values", opLoc))
    }

  | Not => Bool(!isTruthy(right))
  }

and evalConditional = (cond, then, else_) =>
  if isTruthy(cond) {
    evaluate(then)
  } else {
    evaluate(else_)
  }

let execute = (stmt: Stmt.t) =>
  switch stmt.val {
  | Expression(expr) =>
    let _: Expr.value = evaluate(expr)

  | Print(expr) =>
    let value = evaluate(expr)
    Js.log(printValue(value))
  }

let interpret = (program: list<Stmt.t>) =>
  switch List.forEach(program, execute) {
  | () => Ok()
  | exception EvalError(msg, loc) => Error(msg, loc)
  }

