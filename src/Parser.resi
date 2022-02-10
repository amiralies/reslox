type t

let make: array<Location.located<Token.t>> => t

let parse: t => result<list<Stmt.t>, (string, Location.t)>
