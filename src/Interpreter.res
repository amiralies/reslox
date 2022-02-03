exception EvalError(string)

open Expr
let isTruthy = value =>
  switch value {
  | Expr.Nil => false
  | Bool(b) => b
  | Number(_) | String(_) => true
  }

let applyArthimaticOrRaise = (left, right, f) =>
  switch (left, right) {
  | (Number(left), Number(right)) => Number(f(left, right))
  | _ => raise(EvalError("Operands should be numbers"))
  }

let applyComparisonOrRaise = (left, right, p) =>
  switch (left, right) {
  | (Number(left), Number(right)) => Bool(p(left, right))
  | _ => raise(EvalError("Operands should be numbers"))
  }

let rec eval: Expr.t => Expr.value = expr =>
  switch expr {
  | Binary(left, op, right) =>
    let leftValue = eval(left)
    let rightValue = eval(right)
    evalBinary(leftValue, op, rightValue)
  | Grouping(expr) => eval(expr)
  | Literal(value) => value
  | Unary(op, right) =>
    let rightValue = eval(right)

    evalUnary(op, rightValue)
  | Conditional(cond, then, else_) =>
    let condValue = eval(cond)

    evalConditional(condValue, then, else_)
  }

and evalBinary = (left, op, right) =>
  switch op {
  | Sub => applyArthimaticOrRaise(left, right, (l, r) => l -. r)
  | Div => applyArthimaticOrRaise(left, right, (l, r) => l /. r) // TODO Division by zero
  | Mul => applyArthimaticOrRaise(left, right, (l, r) => l *. r)
  | Add =>
    switch (left, right) {
    | (Number(left), Number(right)) => Number(left +. right)
    | (String(left), String(right)) => String(left ++ right)
    | _ => raise(EvalError("Both operands should be numbers or strings"))
    }
  | GreaterThan => applyComparisonOrRaise(left, right, (l, r) => l > r)
  | GreaterThanEqual => applyComparisonOrRaise(left, right, (l, r) => l >= r)
  | LessThan => applyComparisonOrRaise(left, right, (l, r) => l < r)
  | LessThanEqual => applyComparisonOrRaise(left, right, (l, r) => l <= r)
  | Equal => Bool(left == right)
  | NotEqual => Bool(left != right)
  | CommaSequence => right
  }

and evalUnary = (op, right) =>
  switch op {
  | Negative =>
    switch right {
    | Number(number) => Number(-.number)
    | String(_) | Bool(_) | Nil =>
      raise(EvalError("Can't use unary '-' operator on non-number values"))
    }

  | Not => Bool(!isTruthy(right))
  }

and evalConditional = (cond, then, else_) => {
  if isTruthy(cond) {
    eval(then)
  } else {
    eval(else_)
  }
}

let interpret = expr =>
  switch eval(expr) {
  | value => Ok(value)
  | exception EvalError(message) => Error(message)
  }
