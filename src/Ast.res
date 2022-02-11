type value =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil

type rec expr = {
  exprDesc: exprDesc,
  exprLoc: Location.t,
}
and exprDesc =
  | ExprBinary(expr, bop, expr)
  | ExprGrouping(expr)
  | ExprLiteral(value) // TODO located?
  | ExprUnary(uop, expr)
  | ExprConditional(expr, expr, expr)
and bop = {
  bopDesc: bopDesc,
  bopLoc: Location.t,
}
and bopDesc =
  | BopEqual
  | BopNotEqual
  | BopLessThan
  | BopLessThanEqual
  | BopGreaterThan
  | BopGreaterThanEqual
  | BopAdd
  | BopSub
  | BopMul
  | BopDiv
  | BopCommaSequence
and uop = {
  uopDesc: uopDesc,
  uopLoc: Location.t,
}
and uopDesc =
  | UopNegative
  | UopNot

type rec stmt = {
  stmtDesc: stmtDesc,
  stmtLoc: Location.t,
}
and stmtDesc =
  | StmtPrint(expr)
  | StmtExpression(expr)

module Helper = {
  open Location

  module Expr = {
    let mk = (~loc, desc) => {
      exprDesc: desc,
      exprLoc: loc,
    }

    let mkBinary = (left, bop, right) => {
      let loc = {
        start: left.exprLoc.start,
        end: right.exprLoc.end,
      }
      mk(~loc, ExprBinary(left, bop, right))
    }
    let mkGrouping = (~loc, inner) => mk(~loc, ExprGrouping(inner))
    let mkLiteral = (~loc, value) => mk(~loc, ExprLiteral(value))
    let mkUnary = (~loc, uop, right) => mk(~loc, ExprUnary(uop, right))
    let mkConditional = (~loc, condition, then, else_) =>
      mk(~loc, ExprConditional(condition, then, else_))
  }

  module Stmt = {
    let mk = (~loc, desc) => {
      stmtDesc: desc,
      stmtLoc: loc,
    }

    let mkPrint = (~loc, expr) => mk(~loc, StmtPrint(expr))
    let mkExpression = (~loc, expr) => mk(~loc, StmtExpression(expr))
  }
}

