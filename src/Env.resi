type t

let make: (~enclosing: t=?, unit) => t

let define: (t, string, Ast.value) => unit

let get: (t, string) => option<Ast.value>

let assign: (t, string, Ast.value) => result<unit, unit>
