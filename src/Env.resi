type t

let make: unit => t

let define: (t, string, Value.t) => unit

let get: (t, string) => option<Value.t>

let assign: (t, string, Value.t) => result<unit, unit>

