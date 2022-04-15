module H = HashMap.String

type rec t = {
  current: HashMap.String.t<Ast.value>,
  enclosing: option<t>,
}

let make = (~enclosing=?, ()) => {current: H.make(~hintSize=20), enclosing: enclosing}

let define = (t, name, value) => t.current->H.set(name, value)

let rec get = (t, name) =>
  switch t.current->H.get(name) {
  | Some(v) => Some(v)
  | None => t.enclosing->Option.flatMap(get(_, name))
  }

let rec assign = (t, name, value) =>
  if H.has(t.current, name) {
    t.current->H.set(name, value)
    Ok()
  } else {
    switch t.enclosing {
    | None => Error()
    | Some(enc) => enc->assign(name, value)
    }
  }
