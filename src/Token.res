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
  | SemiColon
  | Slash
  | Star
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

type located = {
  typ: t,
  line: int,
}
