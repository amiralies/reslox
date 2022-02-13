module H = HashMap.String

type t = HashMap.String.t<Value.t>

let make = () => H.make(~hintSize=20)

let define = (t, name, value) => t->H.set(name, value)

let get = (t, name) => t->H.get(name)

let assign = (t, name, value) =>
  if H.has(t, name) {
    t->H.set(name, value)
    Ok()
  } else {
    Error()
  }

