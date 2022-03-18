let rec print = expr =>
  switch expr.Ast.exprDesc {
  | Ast.ExprBinary(left, op, right) => parenthesize(printBinaryOp(op), list{left, right})
  | ExprGrouping(expr) => parenthesize("group", list{expr})
  | ExprLiteral(literal) => printLiteralValue(literal)
  | ExprUnary(op, expr) => parenthesize(printUnaryOp(op), list{expr})
  | ExprConditional(condition, thenBRanch, elseBranch) =>
    parenthesize("?:", list{condition, thenBRanch, elseBranch})
  | ExprVariable(name) => name
  | ExprAssign(name, expr) => parenthesize("= " ++ name, list{expr})
  | ExprLogical(left, op, right) => parenthesize(printLogicalOp(op), list{left, right})
  }
and parenthesize = (name: string, exprs: list<Ast.expr>) => {
  let exprsJoined = exprs->List.map(print)->String.concat(" ", _)
  `(${name} ${exprsJoined})`
}
and printBinaryOp = bop =>
  switch bop.bopDesc {
  | BopEqual => "=="
  | BopNotEqual => "!="
  | BopLess => "<"
  | BopLessEqual => "<="
  | BopGreater => ">"
  | BopGreaterEqual => ">="
  | BopAdd => "+"
  | BopSub => "-"
  | BopMul => "*"
  | BopDiv => "/"
  | BopCommaSeq => ","
  }

and printLogicalOp = lop =>
  switch lop.lopDesc {
  | LopOr => "or"
  | LopAnd => "and"
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
