type t<'a>

let empty: t<'a>

let define: (t<'a>, string, 'a) => t<'a>

let get: (t<'a>, Belt.Map.String.key) => option<'a>

let assign: (t<'a>, Belt.Map.String.key, 'a) => result<t<'a>, unit>

let enterBlock: t<'a> => t<'a>

let exitBlock: t<'a> => t<'a>
