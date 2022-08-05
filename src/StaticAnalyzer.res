open Ast

exception AnalyzeError(string, Location.t, option<string>)

module StringId = Id.MakeComparable({
  type t = string
  let cmp = compare
})

type rec context = {
  funKind: inFunStatus,
  classKind: inClassStatus,
  scope: scope,
}
and scope = {
  current: MutableMap.t<string, valueState, StringId.identity>,
  enclosing: option<scope>,
}
and inClassStatus = NoClass | InClass(classKind)
and classKind = Class | SubClass
and inFunStatus = NoFun | InFun(funKind)
and funKind = Fun | Method | Init
and valueState = {defined: bool}

let mkMap = () => MutableMap.make(~id=module(StringId))

let isInGlobalScope = ctx => Option.isNone(ctx.scope.enclosing)

let mkEmptyCtx = () => {
  funKind: NoFun,
  classKind: NoClass,
  scope: {
    current: mkMap(),
    enclosing: None,
  },
}

let enterFunOrMethod = (ctx, kind) => {
  let prevFunkind = ctx.funKind
  let ctx = {...ctx, funKind: InFun(kind)}

  (ctx, prevFunkind)
}

let exitFunOrMethod = (ctx, prevFunKind) => {
  let ctx = {...ctx, funKind: prevFunKind}

  ctx
}

let enterClass = (ctx, kind) => {
  let prevClassKind = ctx.classKind
  let ctx = {...ctx, classKind: InClass(kind)}
  (ctx, prevClassKind)
}

let exitClass = (ctx, prevClassKind) => {
  let ctx = {...ctx, classKind: prevClassKind}

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

    let (ctx, prevFunKind) = enterFunOrMethod(ctx, Fun)
    let ctx = beginScope(ctx)
    List.forEach(params, param => {
      declare(ctx, param.val, param.loc)
      define(ctx, param.val)
    })
    let ctx = resolveStmts(ctx, body)
    let ctx = endScope(ctx)
    let ctx = exitFunOrMethod(ctx, prevFunKind)

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

    let ctx = switch maybeExpr {
    | None => ctx
    | Some(_) if ctx.funKind == InFun(Init) =>
      raise(AnalyzeError("Can't return a value from an initializer.", stmt.stmtLoc, Some("return")))
    | Some(expr) => resolveExpr(ctx, expr)
    }
    ctx

  | StmtWhile(condition, body) =>
    let ctx = resolveExpr(ctx, condition)
    let ctx = resolveStmt(ctx, body)
    ctx

  | StmtClass(name, superclass, methods) =>
    let (ctx, prevClassKind) = enterClass(ctx, superclass == None ? Class : SubClass)

    declare(ctx, name, stmt.stmtLoc)
    define(ctx, name)

    switch superclass {
    | Some(sc) if sc == name =>
      raise(AnalyzeError("A class can't inherit from itself.", stmt.stmtLoc, Some(sc)))

    | None | Some(_) => ()
    }

    let ctx = beginScope(ctx)

    let ctx = List.reduce(methods, ctx, (ctx, method) => {
      let (ctx, prevFunKind) = enterFunOrMethod(ctx, method.name == "init" ? Init : Method)
      let ctx = beginScope(ctx)
      List.forEach(method.parameters, param => {
        declare(ctx, param.val, param.loc)
        define(ctx, param.val)
      })
      let ctx = resolveStmts(ctx, method.body)
      let ctx = endScope(ctx)
      let ctx = exitFunOrMethod(ctx, prevFunKind)

      ctx
    })

    let ctx = endScope(ctx)
    let ctx = exitClass(ctx, prevClassKind)

    ctx
  }

and resolveStmts = (ctx, stmts) => stmts->List.reduce(ctx, resolveStmt)
and declare = (ctx, name, loc) => {
  if ctx.scope.current->MutableMap.has(name) && !isInGlobalScope(ctx) {
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
    | Some({defined: false}) if !isInGlobalScope(ctx) =>
      raise(
        AnalyzeError("Can't read local variable in its own initializer.", expr.exprLoc, Some(name)),
      )

    | None | Some(_) => ctx
    }
    ctx

  | ExprAssign(_, value) =>
    let ctx = resolveExpr(ctx, value)
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
  | ExprConditional(condition, then, else_) =>
    let ctx = resolveExpr(ctx, condition)
    let ctx = resolveExpr(ctx, then)
    let ctx = resolveExpr(ctx, else_)
    ctx

  | ExprGet(object, _) => resolveExpr(ctx, object)
  | ExprSet(object, _, value) =>
    let ctx = resolveExpr(ctx, value)
    let ctx = resolveExpr(ctx, object)
    ctx

  | ExprThis =>
    if ctx.classKind == NoClass {
      raise(AnalyzeError("Can't use 'this' outside of a class.", expr.exprLoc, Some("this")))
    }
    ctx

  | ExprSuper(_) =>
    switch ctx.classKind {
    | NoClass =>
      raise(AnalyzeError("Can't use 'super' outside of a class.", expr.exprLoc, Some("super")))
    | InClass(Class) =>
      raise(
        AnalyzeError(
          "Can't use 'super' in a class with no superclass.",
          expr.exprLoc,
          Some("super"),
        ),
      )
    | InClass(SubClass) => ctx
    }
  }

let analyze = (program: list<Ast.stmt>) => {
  let errors = ref(list{})

  let ctx = ref(mkEmptyCtx())
  List.forEach(program, stmt => {
    try {
      ctx := resolveStmt(ctx.contents, stmt)
    } catch {
    | AnalyzeError(msg, loc, maybeWhere) =>
      let error = (msg, loc, maybeWhere)
      errors := list{error, ...errors.contents}
    }
  })

  switch errors.contents {
  | list{} => Ok()
  | _ => Error(errors.contents->List.reverse)
  }
}
