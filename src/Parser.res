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

let rec expression = parser => commaSequence(parser)
and commaSequence = parser => {
  let expr = conditional(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Comma =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = conditional(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopCommaSequence)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | _ => left
    }

  loop(expr)
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
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopGreaterThan)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | GreaterEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopGreaterThanEqual)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | Less =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopLessThan)
      loop(Ast.Helper.Expr.binary(left, op, right))

    | LessEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      let op = Ast.Helper.Expr.bop(~loc=opLoc, BopLessThanEqual)
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

let _synchronize = parser => {
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

let rec statement = parser =>
  switch peek(parser).val {
  | Print => printStatement(parser)
  | _ => expressionStatement(parser)
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

let parse = parser => {
  let rec loop = acc =>
    if isAtEnd(parser) {
      List.reverse(acc)
    } else {
      let stmtItem = statement(parser)
      loop(list{stmtItem, ...acc})
    }

  switch loop(list{}) {
  | stmts => Ok(stmts)
  | exception ParseError(message, loc) => Error(message, loc)
  }
}

