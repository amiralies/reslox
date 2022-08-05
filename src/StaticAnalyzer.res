open Ast

exception AnalyzeError(string, Location.t, option<string>)

module StringId = Id.MakeComparable({
  type t = string
  let cmp = compare
})

type rec context = {
  funKind: funKind,
  scope: scope,
}
and scope = {
  current: MutableMap.t<string, valueState, StringId.identity>,
  enclosing: option<scope>,
}
and funKind = NoFun | Fun
and valueState = {defined: bool}

let mkMap = () => MutableMap.make(~id=module(StringId))

let mkEmptyCtx = () => {
  funKind: NoFun,
  scope: {
    current: mkMap(),
    enclosing: None,
  },
}

let enterFun = ctx => {
  let prevFunkind = ctx.funKind
  let ctx = {...ctx, funKind: Fun}

  (ctx, prevFunkind)
}

let exitFun = (ctx, prevFunKind) => {
  let ctx = {...ctx, funKind: prevFunKind}

  ctx
}

let beginScope = ctx => {
  ...ctx,
  scope: {
    current: mkMap(),
    enclosing: Some(ctx.scope),
  },
}

let endScope = ctx => {
  ...ctx,
  scope: switch ctx.scope.enclosing {
  | None => failwith("Internal error: exiting without enclosing scope")
  | Some(enclosing) => enclosing
  },
}

let rec resolveStmt = (ctx, stmt) =>
  switch stmt.stmtDesc {
  | StmtBlock(stmts) =>
    let ctx = beginScope(ctx)
    let ctx = resolveStmts(ctx, stmts)
    let ctx = endScope(ctx)
    ctx

  | StmtVar(name, maybeInit) =>
    declare(ctx, name, stmt.stmtLoc)
    let ctx = maybeInit->Option.mapWithDefault(ctx, resolveExpr(ctx))
    define(ctx, name)
    ctx

  | StmtFunction(name, params, body) =>
    declare(ctx, name, stmt.stmtLoc)
    define(ctx, name)

    let (ctx, prevFunKind) = enterFun(ctx)
    let ctx = beginScope(ctx)
    List.forEach(params, param => {
      declare(ctx, param, stmt.stmtLoc)
      define(ctx, param)
    })
    let ctx = resolveStmts(ctx, body)
    let ctx = endScope(ctx)
    let ctx = exitFun(ctx, prevFunKind)

    ctx

  | StmtExpression(expr) => resolveExpr(ctx, expr)

  | StmtIf(condition, then, maybeElse) =>
    let ctx = resolveExpr(ctx, condition)
    let ctx = resolveStmt(ctx, then)
    let ctx = maybeElse->Option.mapWithDefault(ctx, resolveStmt(ctx))
    ctx

  | StmtPrint(expr) => resolveExpr(ctx, expr)

  | StmtReturn(maybeExpr) =>
    if ctx.funKind == NoFun {
      raise(AnalyzeError("Can't return from top-level code.", stmt.stmtLoc, Some("return")))
    }

    let ctx = maybeExpr->Option.mapWithDefault(ctx, resolveExpr(ctx))
    ctx

  | StmtWhile(condition, body) =>
    let ctx = resolveExpr(ctx, condition)
    let ctx = resolveStmt(ctx, body)
    ctx

  | StmtClass(_, _, _) => ctx // TODO
  }

and resolveStmts = (ctx, stmts) => stmts->List.reduce(ctx, resolveStmt)
and declare = (ctx, name, loc) => {
  if ctx.scope.current->MutableMap.has(name) {
    raise(AnalyzeError("Already a variable with this name in this scope.", loc, Some(name)))
  }
  ctx.scope.current->MutableMap.set(name, {defined: false})
}
and define = (ctx, name) => {
  if !(ctx.scope.current->MutableMap.has(name)) {
    failwith("Internal error: define after declare failed")
  }
  ctx.scope.current->MutableMap.set(name, {defined: true})
}
and resolveExpr = (ctx, expr) =>
  switch expr.exprDesc {
  | ExprVariable(name) =>
    let ctx = switch MutableMap.get(ctx.scope.current, name) {
    | Some({defined: false}) =>
      raise(
        AnalyzeError("Can't read local variable in its own initializer.", expr.exprLoc, Some(name)),
      )

    | None | Some({defined: true}) => resolveLocal(ctx, name)
    }
    ctx

  | ExprAssign(name, value) =>
    let ctx = resolveExpr(ctx, value)
    let ctx = resolveLocal(ctx, name)
    ctx

  | ExprBinary(left, _, right) => ctx->resolveExpr(left)->resolveExpr(right)
  | ExprCall(callee, args) =>
    let ctx = resolveExpr(ctx, callee)
    let ctx = List.reduce(args, ctx, resolveExpr)
    ctx

  | ExprGrouping(expr) => resolveExpr(ctx, expr)
  | ExprLiteral(_) => ctx
  | ExprLogical(left, _, right) =>
    let ctx = resolveExpr(ctx, left)
    let ctx = resolveExpr(ctx, right)
    ctx

  | ExprUnary(_, right) => resolveExpr(ctx, right)
  | _ => ctx // TODO
  }
and resolveLocal = (ctx, _name) => {
  // maybe Remove this TODO
  ctx
}

let analyze = (program: list<Ast.stmt>) =>
  try {
    let _: context = List.reduce(program, mkEmptyCtx(), resolveStmt)
    Ok()
  } catch {
  | AnalyzeError(msg, loc, maybeWhere) => Error((msg, loc, maybeWhere))
  }
