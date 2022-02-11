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
  | ExprVariable(string)
and bop = {
  bopDesc: bopDesc,
  bopLoc: Location.t,
}
and bopDesc =
  | BopEqual
  | BopNotEqual
  | BopLess
  | BopLessEqual
  | BopGreater
  | BopGreaterEqual
  | BopAdd
  | BopSub
  | BopMul
  | BopDiv
  | BopCommaSeq
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
  | StmtVar(string, expr)

module Helper = {
  open Location

  module Expr = {
    let mk = (~loc, desc) => {
      exprDesc: desc,
      exprLoc: loc,
    }

    let binary = (left, bop, right) => {
      let loc = {
        start: left.exprLoc.start,
        end: right.exprLoc.end,
      }
      mk(~loc, ExprBinary(left, bop, right))
    }
    let grouping = (~loc, inner) => mk(~loc, ExprGrouping(inner))
    let literal = (~loc, value) => mk(~loc, ExprLiteral(value))
    let unary = (~loc, uop, right) => mk(~loc, ExprUnary(uop, right))
    let conditional = (~loc, condition, then, else_) =>
      mk(~loc, ExprConditional(condition, then, else_))
    let variable = (~loc, name) => mk(~loc, ExprVariable(name))

    let bop = (~loc, desc) => {
      bopDesc: desc,
      bopLoc: loc,
    }

    let uop = (~loc, desc) => {
      uopDesc: desc,
      uopLoc: loc,
    }
  }

  module Stmt = {
    let mk = (~loc, desc) => {
      stmtDesc: desc,
      stmtLoc: loc,
    }

    let print = (~loc, expr) => mk(~loc, StmtPrint(expr))
    let expression = (~loc, expr) => mk(~loc, StmtExpression(expr))
    let var = (~loc, name, expr) => mk(~loc, StmtVar(name, expr))
  }
}

let printValue = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) => Float.toString(f)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  }

