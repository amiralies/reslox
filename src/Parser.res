type t = {
  tokens: array<Location.located<Token.t>>,
  mutable current: int,
}
open Token
open Location

module Helper = Expr.Helpr

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
      loop(Helper.makeBinary(left, {loc: opLoc, val: CommaSequence}, right))

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
      let loc = {start: left.loc.start, end: right.loc.end}
      loop(Helper.makeConditional(~loc, left, middle, right))

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
      loop(Helper.makeBinary(left, {val: NotEqual, loc: opLoc}, right))

    | EqualEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = comparison(parser)
      loop(Helper.makeBinary(left, {val: Equal, loc: opLoc}, right))

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
      loop(Helper.makeBinary(left, {val: GreaterThan, loc: opLoc}, right))

    | GreaterEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, {val: GreaterThanEqual, loc: opLoc}, right))

    | Less =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, {val: LessThan, loc: opLoc}, right))

    | LessEqual =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, {val: LessThanEqual, loc: opLoc}, right))

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
      loop(Helper.makeBinary(left, {val: Add, loc: opLoc}, right))

    | Minus =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = factor(parser)
      loop(Helper.makeBinary(left, {val: Sub, loc: opLoc}, right))

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
      loop(Helper.makeBinary(left, {val: Mul, loc: opLoc}, right))

    | Slash =>
      let opLoc = peek(parser).loc
      advance(parser)
      let right = unary(parser)
      loop(Helper.makeBinary(left, {val: Div, loc: opLoc}, right))

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
    let loc = {start: startLoc, end: right.loc.end}
    Helper.makeUnary(~loc, {val: Not, loc: opLoc}, right)

  | Minus =>
    let opLoc = peek(parser).loc
    let startLoc = opLoc.start
    advance(parser)
    let right = unary(parser)
    let loc = {start: startLoc, end: right.loc.end}
    Helper.makeUnary(~loc, {val: Negative, loc: opLoc}, right)

  | _ => primary(parser)
  }

and primary = parser =>
  switch peek(parser).val {
  | True =>
    let {loc} = peek(parser)
    advance(parser)
    Helper.makeLiteral(~loc, Bool(true))

  | False =>
    let {loc} = peek(parser)
    advance(parser)
    Helper.makeLiteral(~loc, Bool(false))

  | Number(value) =>
    let {loc} = peek(parser)
    advance(parser)
    Helper.makeLiteral(~loc, Number(value))

  | String(value) =>
    let {loc} = peek(parser)
    advance(parser)
    Helper.makeLiteral(~loc, String(value))

  | Nil =>
    let {loc} = peek(parser)
    advance(parser)
    Helper.makeLiteral(~loc, Nil)

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

    Helper.makeGrouping(~loc, expr)

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
  let value = expression(parser)
  let {loc: semiLoc} = consumeIfOrRaise(
    parser,
    peek => peek == SemiColon,
    "Expected ';' after value.",
  )
  let loc = {start: printLoc.start, end: semiLoc.end}

  {val: Stmt.Print(value), loc: loc}
}
and expressionStatement = parser => {
  let value = expression(parser)
  let {loc: semiLoc} = consumeIfOrRaise(
    parser,
    peek => peek == SemiColon,
    "Expected ';' after expression.",
  )
  let loc = {start: value.loc.start, end: semiLoc.end}

  {val: Stmt.Expression(value), loc: loc}
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
  | ast => Ok(ast)
  | exception ParseError(message, loc) => Error(message, loc)
  }
}

