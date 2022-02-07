open Location

type rec t = located<expr>
and expr =
  | Binary(t, binaryOperator, t)
  | Grouping(t)
  | Literal(value)
  | Unary(unaryOperator, t)
  | Conditional(t, t, t)
and value =
  | String(string)
  | Number(float)
  | Bool(bool)
  | Nil
and binaryOperator =
  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Add
  | Sub
  | Mul
  | Div
  | CommaSequence
and unaryOperator = Negative | Not

let printValue = value =>
  switch value {
  | String(s) => s
  | Number(f) => Float.toString(f)
  | Bool(b) => b ? "true" : "false"
  | Nil => "nil"
  }

module Helpr = {
  let make = (~loc, expr) => {val: expr, loc: loc}

  let makeBinary = (~loc, left, op, right) => make(~loc, Binary(left, right, op))
  let makeGrouping = (~loc, inner) => make(~loc, Grouping(inner))
  let makeLiteral = (~loc, value) => make(~loc, Literal(value))
  let makeUnary = (~loc, op, right) => make(~loc, Unary(op, right))
  let makeConditional = (~loc, condition, then, else_) =>
    make(~loc, Conditional(condition, then, else_))
}
