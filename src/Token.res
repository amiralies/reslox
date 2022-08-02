type t =
  // Single char tokens
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Colon
  | SemiColon
  | Slash
  | Star
  | Question
  // One or two character tokens
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  // Literals
  | Identifier(string)
  | String(string)
  | Number(float)
  // Keywords
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  // EOF
  | EOF

let keywordMap =
  [
    ("and", And),
    ("class", Class),
    ("else", Else),
    ("false", False),
    ("for", For),
    ("fun", Fun),
    ("if", If),
    ("nil", Nil),
    ("or", Or),
    ("print", Print),
    ("return", Return),
    ("super", Super),
    ("this", This),
    ("true", True),
    ("var", Var),
    ("while", While),
  ]->Map.String.fromArray

let toString = t =>
  switch t {
  | LeftParen => "LeftParen"
  | RightParen => "RightParen"
  | LeftBrace => "LeftBrace"
  | RightBrace => "RightBrace"
  | Comma => "Comma"
  | Dot => "Dot"
  | Minus => "Minus"
  | Plus => "Plus"
  | Colon => "Colon"
  | SemiColon => "SemiColon"
  | Slash => "Slash"
  | Star => "Star"
  | Question => "Question"
  | Bang => "Bang"
  | BangEqual => "BangEqual"
  | Equal => "Equal"
  | EqualEqual => "EqualEqual"
  | Greater => "Greater"
  | GreaterEqual => "GreaterEqual"
  | Less => "Less"
  | LessEqual => "LessEqual"
  | Identifier(name) => "Identifier(" ++ name ++ ")"
  | String(string) => `String("${string}")`
  | Number(float) => `Number(${Float.toString(float)})`
  | And => "And"
  | Class => "Class"
  | Else => "Else"
  | False => "False"
  | Fun => "Fun"
  | For => "For"
  | If => "If"
  | Nil => "Nil"
  | Or => "Or"
  | Print => "Print"
  | Return => "Return"
  | Super => "Super"
  | This => "This"
  | True => "True"
  | Var => "Var"
  | While => "While"
  | EOF => "EOF"
  }
let toLexeme = t =>
  switch t {
  | EOF => ""
  | LeftParen => "("
  | RightParen => ")"
  | LeftBrace => "{"
  | RightBrace => "}"
  | Comma => ","
  | Dot => "."
  | Minus => "-"
  | Plus => "+"
  | Colon => ":"
  | SemiColon => ";"
  | Slash => "/"
  | Star => "*"
  | Question => "?"
  | Bang => "!"
  | BangEqual => "!="
  | Equal => "="
  | EqualEqual => "=="
  | Greater => ">"
  | GreaterEqual => ">="
  | Less => "<"
  | LessEqual => "<="
  | And => "and"
  | Class => "class"
  | Else => "else"
  | False => "false"
  | Fun => "fun"
  | For => "for"
  | If => "if"
  | Nil => "nil"
  | Or => "or"
  | Print => "print"
  | Return => "return"
  | Super => "super"
  | This => "this"
  | True => "true"
  | Var => "var"
  | While => "while"
  | Identifier(ident) => ident
  | String(s) => s
  | Number(f) => Float.toString(f)
  }
