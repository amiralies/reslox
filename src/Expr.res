type rec t =
  | Binary(t, binaryOperator, t)
  | Grouping(t)
  | Literal(literal)
  | Unary(unaryOperator, t)
and literal =
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
and unaryOperator = Negative | Not
