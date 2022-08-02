type t

let make: array<Location.located<Token.t>> => t

let parse: t => result<list<Ast.stmt>, list<(string, Location.located<Token.t>)>>
