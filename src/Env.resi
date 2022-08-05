type t

let empty: t

let define: (t, string, Value.t) => t

let get: (t, string) => option<Value.t>

let assign: (t, string,Value.t) => result<unit, unit>

let enterBlock: t => t

let exitBlock: t => t
