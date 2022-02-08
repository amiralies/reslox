open Location

let rec print = expr =>
  switch expr.val {
  | Expr.Binary(left, op, right) => parenthesize(printBinaryOp(op), list{left, right})
  | Grouping(expr) => parenthesize("group", list{expr})
  | Literal(literal) => printLiteral(literal)
  | Unary(op, expr) => parenthesize(printUnaryOp(op), list{expr})
  | Conditional(condition, then, else_) => parenthesize("?:", list{condition, then, else_})
  }
and parenthesize = (name: string, exprs: list<Expr.t>) => {
  let exprsJoined = exprs->List.map(print)->String.concat(" ", _)
  `(${name} ${exprsJoined})`
}
and printBinaryOp = op =>
  switch op.val {
  | Equal => "=="
  | NotEqual => "!="
  | LessThan => "<"
  | LessThanEqual => "<="
  | GreaterThan => ">"
  | GreaterThanEqual => ">="
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
  | CommaSequence => ","
  }

and printLiteral = literal =>
  switch literal.val {
  | String(str) => `"${str}"`
  | Number(number) => Float.toString(number)
  | Bool(b) => b ? "true" : "false"
  | Nil => "nil"
  }
and printUnaryOp = op =>
  switch op.val {
  | Negative => "-"
  | Not => "!"
  }
