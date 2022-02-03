type rec t =
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
