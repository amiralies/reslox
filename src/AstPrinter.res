let rec print = expr =>
  switch expr.Ast.exprDesc {
  | Ast.ExprBinary(left, op, right) => parenthesize(printBinaryOp(op), list{left, right})
  | ExprGrouping(expr) => parenthesize("group", list{expr})
  | ExprLiteral(literal) => printLiteralValue(literal)
  | ExprUnary(op, expr) => parenthesize(printUnaryOp(op), list{expr})
  | ExprConditional(condition, then, else_) => parenthesize("?:", list{condition, then, else_})
  }
and parenthesize = (name: string, exprs: list<Ast.expr>) => {
  let exprsJoined = exprs->List.map(print)->String.concat(" ", _)
  `(${name} ${exprsJoined})`
}
and printBinaryOp = bop =>
  switch bop.bopDesc {
  | BopEqual => "=="
  | BopNotEqual => "!="
  | BopLessThan => "<"
  | BopLessThanEqual => "<="
  | BopGreaterThan => ">"
  | BopGreaterThanEqual => ">="
  | BopAdd => "+"
  | BopSub => "-"
  | BopMul => "*"
  | BopDiv => "/"
  | BopCommaSequence => ","
  }

and printLiteralValue = value =>
  switch value {
  | VString(str) => `"${str}"`
  | VNumber(number) => Float.toString(number)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  }
and printUnaryOp = uop =>
  switch uop.uopDesc {
  | UopNegative => "-"
  | UopNot => "!"
  }

