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
exception ParseError(string)

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
    raise(ParseError(message))
  }

let rec expression = parser => commaSequence(parser)
and commaSequence = parser => {
  let expr = conditional(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Comma =>
      advance(parser)
      let right = conditional(parser)
      loop(Helper.makeBinary(left, CommaSequence, right))

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
      advance(parser)
      let right = comparison(parser)
      loop(Helper.makeBinary(left, NotEqual, right))

    | EqualEqual =>
      advance(parser)
      let right = comparison(parser)
      loop(Helper.makeBinary(left, Equal, right))

    | _ => left
    }

  loop(expr)
}
and comparison = parser => {
  let expr = term(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Greater =>
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, GreaterThan, right))

    | GreaterEqual =>
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, GreaterThanEqual, right))

    | Less =>
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, LessThan, right))

    | LessEqual =>
      advance(parser)
      let right = term(parser)
      loop(Helper.makeBinary(left, LessThanEqual, right))

    | _ => left
    }

  loop(expr)
}
and term = parser => {
  let expr = factor(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Plus =>
      advance(parser)
      let right = factor(parser)
      loop(Helper.makeBinary(left, Add, right))

    | Minus =>
      advance(parser)
      let right = factor(parser)
      loop(Helper.makeBinary(left, Sub, right))

    | _ => left
    }

  loop(expr)
}
and factor = parser => {
  let expr = unary(parser)

  let rec loop = left =>
    switch peek(parser).val {
    | Star =>
      advance(parser)
      let right = unary(parser)
      loop(Helper.makeBinary(left, Mul, right))

    | Slash =>
      advance(parser)
      let right = unary(parser)
      loop(Helper.makeBinary(left, Div, right))

    | _ => left
    }

  loop(expr)
}
and unary = parser =>
  switch peek(parser).val {
  | Bang =>
    let startLoc = peek(parser).loc.start
    advance(parser)
    let right = unary(parser)
    let loc = {start: startLoc, end: right.loc.end}
    Helper.makeUnary(~loc, Not, right)

  | Minus =>
    let startLoc = peek(parser).loc.start
    advance(parser)
    let right = unary(parser)
    let loc = {start: startLoc, end: right.loc.end}
    Helper.makeUnary(~loc, Negative, right)

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

  | _ => raise(ParseError("Expected expression"))
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

let parse = parser =>
  switch expression(parser) {
  | ast => Ok(ast)
  | exception ParseError(parseError) => Error(parseError)
  }
