type rec expr = {
  exprDesc: exprDesc,
  exprLoc: Location.t,
}
and exprDesc =
  | ExprBinary(expr, bop, expr)
  | ExprGrouping(expr)
  | ExprLiteral(Value.t) // TODO located?
  | ExprUnary(uop, expr)
  | ExprConditional(expr, expr, expr)
  | ExprVariable(string)
  | ExprAssign(string, expr)
  | ExprLogical(expr, lop, expr)
  | ExprCall(expr, list<expr>)
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
and uop = {
  uopDesc: uopDesc,
  uopLoc: Location.t,
}
and uopDesc =
  | UopNegative
  | UopNot
and lop = {
  lopDesc: lopDesc,
  lopLoc: Location.t,
}
and lopDesc =
  | LopOr
  | LopAnd

type rec stmt = {
  stmtDesc: stmtDesc,
  stmtLoc: Location.t,
}
and stmtDesc =
  | StmtPrint(expr)
  | StmtExpression(expr)
  | StmtVar(string, expr)
  | StmtBlock(list<stmt>)
  | StmtIf(expr, stmt, option<stmt>)
  | StmtWhile(expr, stmt)
  | StmtFunction(string, list<string>, list<stmt>)
  | StmtReturn(option<expr>)

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
    let conditional = (~loc, condition, thenBranch, elseBranch) =>
      mk(~loc, ExprConditional(condition, thenBranch, elseBranch))
    let variable = (~loc, name) => mk(~loc, ExprVariable(name))
    let assign = (~loc, name, expr) => mk(~loc, ExprAssign(name, expr))
    let logical = (left, lop, right) => {
      let loc = {
        start: left.exprLoc.start,
        end: right.exprLoc.end,
      }
      mk(~loc, ExprLogical(left, lop, right))
    }
    let call = (~loc, callee, args) => mk(~loc, ExprCall(callee, args))

    let bop = (~loc, desc) => {
      bopDesc: desc,
      bopLoc: loc,
    }

    let uop = (~loc, desc) => {
      uopDesc: desc,
      uopLoc: loc,
    }

    let lop = (~loc, desc) => {
      lopDesc: desc,
      lopLoc: loc,
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
    let block = (~loc, items) => mk(~loc, StmtBlock(items))
    let if_ = (~loc, condition, thenBranch, elseBranch) =>
      mk(~loc, StmtIf(condition, thenBranch, elseBranch))
    let while_ = (~loc, condition, body) => mk(~loc, StmtWhile(condition, body))
    let function = (~loc, name, parameters, body) => mk(~loc, StmtFunction(name, parameters, body))
    let return = (~loc, maybeExpr) => mk(~loc, StmtReturn(maybeExpr))
  }
}
