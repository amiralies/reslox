type t = {
  tokens: array<Token.located>,
  mutable current: int,
}
open Token

let make = tokens => {
  tokens: tokens,
  current: 0,
}

// type parseError = ParseError
exception ParseError(string)

let peek = ({tokens, current}) => Array.getUnsafe(tokens, current)

let isAtEnd = parser => peek(parser).typ == EOF

let advance = parser =>
  if !isAtEnd(parser) {
    parser.current = parser.current + 1
  }

let consumeIfOrRaise = (parser, p, message) =>
  if p(peek(parser).typ) {
    advance(parser)
  } else {
    raise(ParseError(message))
  }

let rec expression = parser => commaSequence(parser)
and commaSequence = parser => {
  let expr = equality(parser)

  let rec loop = left =>
    switch peek(parser).typ {
    | Comma =>
      advance(parser)
      let right = equality(parser)
      loop(Expr.Binary(left, CommaSequence, right))

    | _ => left
    }

  loop(expr)
}
and equality = parser => {
  let expr = comparison(parser)

  let rec loop = left =>
    switch peek(parser).typ {
    | BangEqual =>
      advance(parser)
      let right = comparison(parser)
      loop(Expr.Binary(left, NotEqual, right))

    | EqualEqual =>
      advance(parser)
      let right = comparison(parser)
      loop(Expr.Binary(left, Equal, right))

    | _ => left
    }

  loop(expr)
}
and comparison = parser => {
  let expr = term(parser)

  let rec loop = left =>
    switch peek(parser).typ {
    | Greater =>
      advance(parser)
      let right = term(parser)
      loop(Expr.Binary(left, GreaterThan, right))

    | GreaterEqual =>
      advance(parser)
      let right = term(parser)
      loop(Expr.Binary(left, GreaterThanEqual, right))

    | Less =>
      advance(parser)
      let right = term(parser)
      loop(Expr.Binary(left, LessThan, right))

    | LessEqual =>
      advance(parser)
      let right = term(parser)
      loop(Expr.Binary(left, LessThanEqual, right))

    | _ => left
    }

  loop(expr)
}
and term = parser => {
  let expr = factor(parser)

  let rec loop = left =>
    switch peek(parser).typ {
    | Plus =>
      advance(parser)
      let right = factor(parser)
      loop(Expr.Binary(left, Add, right))

    | Minus =>
      advance(parser)
      let right = factor(parser)
      loop(Expr.Binary(left, Sub, right))

    | _ => left
    }

  loop(expr)
}
and factor = parser => {
  let expr = unary(parser)

  let rec loop = left =>
    switch peek(parser).typ {
    | Star =>
      advance(parser)
      let right = unary(parser)
      loop(Expr.Binary(left, Mul, right))

    | Slash =>
      advance(parser)
      let right = unary(parser)
      loop(Expr.Binary(left, Div, right))

    | _ => left
    }

  loop(expr)
}
and unary = parser =>
  switch peek(parser).typ {
  | Bang =>
    advance(parser)
    let right = unary(parser)
    Expr.Unary(Not, right)

  | Minus =>
    advance(parser)
    let right = unary(parser)
    Expr.Unary(Negative, right)

  | _ => primary(parser)
  }

and primary = parser =>
  switch peek(parser).typ {
  | True =>
    advance(parser)
    Expr.Literal(Bool(true))

  | False =>
    advance(parser)
    Expr.Literal(Bool(false))

  | Number(value) =>
    advance(parser)
    Expr.Literal(Number(value))

  | String(value) =>
    advance(parser)
    Expr.Literal(String(value))

  | Nil =>
    advance(parser)
    Expr.Literal(Nil)
  | LeftParen =>
    advance(parser)
    let expr = expression(parser)
    consumeIfOrRaise(parser, peek => peek == RightParen, "Expected a right paren")
    Expr.Grouping(expr)

  | _ => raise(ParseError("Expected expression"))
  }

let _synchronize = parser => {
  advance(parser)

  let rec loop = parser =>
    if isAtEnd(parser) {
      ()
    } else {
      let maybePrevToken = parser.tokens[parser.current - 1]->Option.map(({typ}) => typ)
      if maybePrevToken == Some(SemiColon) {
        ()
      } else {
        switch peek(parser).typ {
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
