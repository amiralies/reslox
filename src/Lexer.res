open Token

type lexer = {
  mutable startIndex: int,
  mutable currentIndex: int,
  mutable startLine: int,
  mutable currentLine: int,
  mutable startCol: int,
  mutable currentCol: int,
  source: string,
}

type lexerError =
  | UnknownChar
  | UnterminatedString
  | UnterminatedMultiLineComment

type singleScanResult =
  | LocatedToken(Location.located<Token.t>)
  | SingleScanError(lexerError)
  | Skip
  | EndReached

let makeLexer = source => {
  startIndex: 0,
  currentIndex: 0,
  currentLine: 1,
  startLine: 1,
  currentCol: 1,
  startCol: 1,
  source: source,
}

let isAtEnd = lexer => lexer.currentIndex >= String.length(lexer.source)

let peek = lexer =>
  switch String.get(lexer.source, lexer.currentIndex) {
  | c => Some(c)
  | exception _ => None
  }

let peekNext = lexer =>
  switch String.get(lexer.source, lexer.currentIndex + 1) {
  | c => Some(c)
  | exception _ => None
  }

let advance = lexer =>
  switch peek(lexer) {
  | Some(_) as maybeCurrentChar =>
    lexer.currentIndex = lexer.currentIndex + 1
    lexer.currentCol = lexer.currentCol + 1
    maybeCurrentChar
  | None => None
  }

let advanceDiscard = lexer => {
  let _: option<char> = advance(lexer)
}

let advanceLineOnly = lexer => {
  lexer.currentLine = lexer.currentLine + 1
  lexer.currentCol = 1
}

let match = (expected, lexer) =>
  switch peek(lexer) {
  | Some(c) if c == expected =>
    lexer.currentIndex = lexer.currentIndex + 1

    true
  | Some(_) | None => false
  }

let emitLocatedToken = (token, lexer) => LocatedToken({
  val: token,
  loc: {
    start: {
      line: lexer.startLine,
      col: lexer.startCol,
    },
    end: {
      line: lexer.currentLine,
      col: lexer.currentCol - 1,
    },
  },
})

let isDigit = c =>
  switch c {
  | '0' .. '9' => true
  | _ => false
  }

let isAlpha = c =>
  switch c {
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' => true
  | _ => false
  }

let isAlphaNumeric = c => isAlpha(c) || isDigit(c)

let rec skipTilLineBreak = lexer =>
  switch peek(lexer) {
  | None | Some('\n') => ()
  | Some(_) =>
    advanceDiscard(lexer)
    skipTilLineBreak(lexer)
  }

let skipMultiLineComment = lexer => {
  let rec aux = (level, lexer) => {
    switch peek(lexer) {
    | Some('*') if peekNext(lexer) == Some('/') =>
      advanceDiscard(lexer)
      advanceDiscard(lexer)
      if level == 0 {
        Skip
      } else {
        aux(level - 1, lexer)
      }
    | Some('/') if peekNext(lexer) == Some('*') =>
      advanceDiscard(lexer)
      advanceDiscard(lexer)
      aux(level + 1, lexer)
    | Some(c) =>
      advanceDiscard(lexer)
      if c == '\n' {
        advanceLineOnly(lexer)
      }
      aux(level, lexer)
    | None => SingleScanError(UnterminatedMultiLineComment)
    }
  }

  aux(0, lexer)
}

let rec scanString = lexer =>
  switch peek(lexer) {
  | None => SingleScanError(UnterminatedString)
  | Some('"') =>
    let value = StringLabels.sub(
      lexer.source,
      ~len=lexer.currentIndex - lexer.startIndex - 1,
      ~pos=lexer.startIndex + 1,
    )
    advanceDiscard(lexer)
    emitLocatedToken(String(value), lexer)
  | Some(c) =>
    advanceDiscard(lexer)
    if c == '\n' {
      advanceLineOnly(lexer)
    }
    scanString(lexer)
  }

let scanNumber = lexer => {
  let rec skipIntegerPart = lexer =>
    switch peek(lexer) {
    | Some(c) if isDigit(c) =>
      advanceDiscard(lexer)
      skipIntegerPart(lexer)
    | Some(_)
    | None => ()
    }
  skipIntegerPart(lexer)

  switch (peek(lexer), peekNext(lexer)) {
  | (Some('.'), Some(next)) if isDigit(next) =>
    // consume '.'
    advanceDiscard(lexer)

    skipIntegerPart(lexer)
  | _ => ()
  }

  let rawValue = StringLabels.sub(
    lexer.source,
    ~pos=lexer.startIndex,
    ~len=lexer.currentIndex - lexer.startIndex,
  )
  let value = rawValue->Float.fromString->Option.getExn
  emitLocatedToken(Number(value), lexer)
}

let rec scanIdentOrKeyword = lexer =>
  switch peek(lexer) {
  | Some(c) if isAlphaNumeric(c) =>
    advanceDiscard(lexer)
    scanIdentOrKeyword(lexer)
  | Some(_)
  | None =>
    let value = StringLabels.sub(
      lexer.source,
      ~pos=lexer.startIndex,
      ~len=lexer.currentIndex - lexer.startIndex,
    )

    let token = switch Map.String.get(Token.keywordMap, value) {
    | Some(keyword) => keyword
    | None => Identifier(value)
    }

    emitLocatedToken(token, lexer)
  }

let scanToken = lexer =>
  lexer
  ->advance
  ->Option.mapWithDefault(EndReached, c =>
    switch c {
    | '(' => LeftParen->emitLocatedToken(lexer)
    | ')' => RightParen->emitLocatedToken(lexer)
    | '{' => LeftBrace->emitLocatedToken(lexer)
    | '}' => RightBrace->emitLocatedToken(lexer)
    | ',' => Comma->emitLocatedToken(lexer)
    | '.' => Dot->emitLocatedToken(lexer)
    | '-' => Minus->emitLocatedToken(lexer)
    | '+' => Plus->emitLocatedToken(lexer)
    | ':' => Colon->emitLocatedToken(lexer)
    | ';' => SemiColon->emitLocatedToken(lexer)
    | '*' => Star->emitLocatedToken(lexer)
    | '?' => Question->emitLocatedToken(lexer)
    | '!' =>
      if match('=', lexer) {
        BangEqual->emitLocatedToken(lexer)
      } else {
        Bang->emitLocatedToken(lexer)
      }
    | '=' =>
      if match('=', lexer) {
        EqualEqual->emitLocatedToken(lexer)
      } else {
        Equal->emitLocatedToken(lexer)
      }
    | '<' =>
      if match('=', lexer) {
        LessEqual->emitLocatedToken(lexer)
      } else {
        Less->emitLocatedToken(lexer)
      }
    | '>' =>
      if match('=', lexer) {
        GreaterEqual->emitLocatedToken(lexer)
      } else {
        Greater->emitLocatedToken(lexer)
      }
    | '/' =>
      if match('/', lexer) {
        let () = skipTilLineBreak(lexer)
        Skip
      } else if match('*', lexer) {
        skipMultiLineComment(lexer)
      } else {
        Slash->emitLocatedToken(lexer)
      }
    | ' ' | '\t' | '\r' => Skip
    | '\n' =>
      advanceLineOnly(lexer)
      Skip

    | '"' => scanString(lexer)
    | c if isDigit(c) => scanNumber(lexer)
    | c if isAlpha(c) => scanIdentOrKeyword(lexer)
    | _ => SingleScanError(UnknownChar)
    }
  )

let scanTokens: string => result<list<Location.located<Token.t>>, lexerError> = source => {
  let lexer = makeLexer(source)
  let rec loop = acc => {
    lexer.startIndex = lexer.currentIndex
    lexer.startLine = lexer.currentLine
    lexer.startCol = lexer.currentCol

    switch scanToken(lexer) {
    | SingleScanError(err) => Error(err)
    | LocatedToken(locatedToken) => loop(list{locatedToken, ...acc})
    | Skip => loop(acc)
    | EndReached =>
      let eof = {
        Location.val: EOF,
        loc: {
          start: {
            col: lexer.currentCol,
            line: lexer.currentLine,
          },
          end: {
            col: lexer.currentCol,
            line: lexer.currentLine,
          },
        },
      }
      let tokens = list{eof, ...acc}->List.reverse

      Ok(tokens)
    }
  }

  loop(list{})
}
