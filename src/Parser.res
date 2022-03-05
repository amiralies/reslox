type t = {
  tokens: array<Location.located<Token.t>>,
  mutable current: int,
}
open Token
open Location
open Ast

let make = tokens => {
  tokens: tokens,
  current: 0,
}

// type parseError = ParseError
exception ParseError(string, Location.t)

let peek = ({tokens, current}) => Array.getUnsafe(tokens, current)

let isAtEnd = parser => peek(parser).val == EOF

let advance = parser =>
  if !isAtEnd(parser) {
    parser.current = parser.current + 1
  }

let consumeIfOrRaise = (parser, p, message) =>
  if p(peek(parser).val) {
    let token = peek(parser)
    advance(parser)
    token
  } else {
    raise(ParseError(message, peek(parser).loc))
  }

let consumeMapOrRaise = (parser, f, message) =>
  switch f(peek(parser).val) {
  | Some(v) =>
    let {loc} = peek(parser)
    advance(parser)
    {val: v, loc: loc}
  | None => raise(ParseError(message, peek(parser).loc))
  }

let rec expression = parser => commaSequence(parser)
and commaSequence = parser => {
  let expr = assignment(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Comma =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = assignment(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopCommaSeq)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | _ => left
    }

  loop(expr)
}
and assignment = parser => {
  let expr = conditional(parser)

  switch peek(parser).val {
  | Equal =>
    advance(parser)
    switch expr.exprDesc {
    | ExprVariable(name) =>
      let rhsExpr = assignment(parser)
      let loc = {start: expr.exprLoc.start, end: rhsExpr.exprLoc.end}

      Ast.Helper.Expr.assign(~loc, name, rhsExpr)
    | _ => raise(ParseError("Invalid assignment target.", expr.exprLoc))
    }
  | _ => expr
  }
}
and conditional = parser => {
  let expr = equality(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Question =>
      advance(parser)
      let middle = conditional(parser)
      let _: Location.located<Token.t> = consumeIfOrRaise(
        parser,
        peek => peek == Colon,
        "Expected ':' after expression",
      )
      let right = conditional(parser)
      let loc = {start: left.Ast.exprLoc.start, end: right.exprLoc.end}
      loop(Ast.Helper.Expr.conditional(~loc, left, middle, right))

    | _ => left
    }

  loop(expr)
}
and equality = parser => {
  let expr = comparison(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | BangEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = comparison(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopNotEqual)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | EqualEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = comparison(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopEqual)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | _ => left
    }

  loop(expr)
}
and comparison = parser => {
  let expr = term(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Greater =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopGreater)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | GreaterEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopGreaterEqual)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | Less =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopLess)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | LessEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopLessEqual)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | _ => left
    }

  loop(expr)
}
and term = parser => {
  let expr = factor(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Plus =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = factor(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopAdd)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | Minus =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = factor(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopSub)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | _ => left
    }

  loop(expr)
}
and factor = parser => {
  let expr = unary(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Star =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = unary(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopMul)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | Slash =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = unary(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopDiv)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | _ => left
    }

  loop(expr)
}
and unary = parser =>
  switch peek(parser).val {
  | Bang =>
    let opLoc = peek(parser).loc
    let startLoc = opLoc.start
    advance(parser)
    let right = unary(parser)
    let loc = {start: startLoc, end: right.exprLoc.end}
    let op = Ast.Helper.Expr.uop(~loc=opLoc, UopNot)
    Ast.Helper.Expr.unary(~loc, op, right)

  | Minus =>
    let opLoc = peek(parser).loc
    let startLoc = opLoc.start
    advance(parser)
    let right = unary(parser)
    let loc = {start: startLoc, end: right.exprLoc.end}
    let op = Ast.Helper.Expr.uop(~loc=opLoc, UopNegative)
    Ast.Helper.Expr.unary(~loc, op, right)

  | _ => primary(parser)
  }

and primary = parser =>
  switch peek(parser).val {
  | True =>
    let {loc} = peek(parser)
    advance(parser)
    Ast.Helper.Expr.literal(~loc, VBool(true))

  | False =>
    let {loc} = peek(parser)
    advance(parser)
    Ast.Helper.Expr.literal(~loc, VBool(false))

  | Number(value) =>
    let {loc} = peek(parser)
    advance(parser)
    Ast.Helper.Expr.literal(~loc, VNumber(value))

  | String(value) =>
    let {loc} = peek(parser)
    advance(parser)
    Ast.Helper.Expr.literal(~loc, VString(value))

  | Nil =>
    let {loc} = peek(parser)
    advance(parser)
    Ast.Helper.Expr.literal(~loc, VNil)

  | Identifier(name) =>
    let {loc} = peek(parser)
    advance(parser)
    Ast.Helper.Expr.variable(~loc, name)

  | LeftParen =>
    let {loc: {start: startLoc}} = peek(parser)
    advance(parser)
    let expr = expression(parser)
    let consumedRParen = consumeIfOrRaise(
      parser,
      peek => peek == RightParen,
      "Expected a right paren",
    )
    let endLoc = consumedRParen.loc.end
    let loc = {start: startLoc, end: endLoc}

    Ast.Helper.Expr.grouping(~loc, expr)

  | _ => raise(ParseError("Expected expression", peek(parser).loc))
  }

let synchronize = parser => {
  advance(parser)

  let rec loop = parser =>
    if isAtEnd(parser) {
      ()
    } else {
      let maybePrevToken = parser.tokens[parser.current - 1]->Option.map(({val}) => val)
      if maybePrevToken == Some(SemiColon) {
        ()
      } else {
        switch peek(parser).val {
        | Class
        | Fun
        | Var
        | For
        | If
        | While
        | Print
        | Return => ()
        | _ =>
          advance(parser)
          loop(parser)
        }
      }
    }

  loop(parser)
}

let rec declaration = parser => {
  try Ok(
    switch peek(parser).val {
    | Var => varDeclaration(parser)
    | _ => statement(parser)
    },
  ) catch {
  | ParseError(msg, loc) =>
    synchronize(parser)
    Error(msg, loc)
  }
}
and varDeclaration = parser => {
  let varLoc = peek(parser).loc
  advance(parser) // Consume var token
  let {val: name, loc: nameLoc} = consumeMapOrRaise(
    parser,
    token =>
      switch token {
      | Identifier(name) => Some(name)
      | _ => None
      },
    "Expect variable name.",
  )

  let initilizer = switch peek(parser).val {
  | Equal =>
    advance(parser)
    expression(parser)
  | _ => Ast.Helper.Expr.literal(~loc={start: nameLoc.start, end: nameLoc.end}, VNil)
  }

  let {loc: semiLoc} = consumeIfOrRaise(
    parser,
    peek => peek == SemiColon,
    "Expect ';' after variable declaration.",
  )

  let loc = {start: varLoc.start, end: semiLoc.end}

  Ast.Helper.Stmt.var(~loc, name, initilizer)
}
and statement = parser =>
  switch peek(parser).val {
  | If => ifStatement(parser)
  | Print => printStatement(parser)
  | LeftBrace => blockStatement(parser)
  | _ => expressionStatement(parser)
  }
and ifStatement = parser => {
  let ifLoc = peek(parser).loc
  advance(parser) // Consume if token
  let _ = consumeIfOrRaise(parser, peek => peek == LeftParen, "Expect '(' after 'if'.")
  let condition = expression(parser)
  let _ = consumeIfOrRaise(parser, peek => peek == RightParen, "Expect ')' after 'if' condition.")

  let thenBranch = statement(parser)

  let elseBranch = switch peek(parser).val {
  | Else =>
    advance(parser) // Consume else token

    let elseStmt = statement(parser)
    Some(elseStmt)
  | _ => None
  }

  let endingStmt = Option.getWithDefault(elseBranch, thenBranch)
  let loc = {start: ifLoc.start, end: endingStmt.stmtLoc.end}

  Ast.Helper.Stmt.if_(~loc, condition, thenBranch, elseBranch)
}
and printStatement = parser => {
  let printLoc = peek(parser).loc
  advance(parser) // Consume print token
  let expr = expression(parser)
  let {loc: semiLoc} = consumeIfOrRaise(
    parser,
    peek => peek == SemiColon,
    "Expected ';' after value.",
  )
  let loc = {start: printLoc.start, end: semiLoc.end}

  Ast.Helper.Stmt.print(~loc, expr)
}
and expressionStatement = parser => {
  let expr = expression(parser)
  let {loc: semiLoc} = consumeIfOrRaise(
    parser,
    peek => peek == SemiColon,
    "Expected ';' after expression.",
  )
  let loc = {start: expr.exprLoc.start, end: semiLoc.end}

  Ast.Helper.Stmt.expression(~loc, expr)
}

and blockStatement = parser => {
  let leftBraceLoc = peek(parser).loc
  advance(parser) // Consume leftbrace token

  let rec loop = acc =>
    if peek(parser).val == RightBrace || isAtEnd(parser) {
      List.reverse(acc)
    } else {
      let item = declaration(parser)
      switch item {
      | Ok(item) => loop(list{item, ...acc})
      | Error(msg, loc) => raise(ParseError(msg, loc))
      }
    }

  let items = loop(list{})

  let {loc: rightBraceLoc} = consumeIfOrRaise(
    parser,
    peek => peek == RightBrace,
    "Expect '}' after block.",
  )
  let loc = {start: leftBraceLoc.start, end: rightBraceLoc.end}

  Ast.Helper.Stmt.block(~loc, items)
}

let parse = parser => {
  let rec loop = acc =>
    if isAtEnd(parser) {
      List.reverse(acc)
    } else {
      let stmtItemOrErr = declaration(parser)
      loop(list{stmtItemOrErr, ...acc})
    }

  let stmtItemsAndErrors = loop(list{})
  let stmtItems = stmtItemsAndErrors->List.keepMap(itemOrErr =>
    switch itemOrErr {
    | Ok(item) => Some(item)
    | Error(_) => None
    }
  )

  let errorItems = stmtItemsAndErrors->List.keepMap(itemOrErr =>
    switch itemOrErr {
    | Error(err) => Some(err)
    | Ok(_) => None
    }
  )

  if errorItems == list{} {
    Ok(stmtItems)
  } else {
    Error(errorItems)
  }
}
