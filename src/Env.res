type rec t<'a> = {
  current: Map.String.t<ref<'a>>,
  enclosing: option<t<'a>>,
}

let empty = {
  current: Map.String.empty,
  enclosing: None,
}

let define = (t, name, value) => {
  enclosing: t.enclosing,
  current: t.current->Map.String.set(name, ref(value)),
}

let rec get = (t, name) =>
  switch t.current->Map.String.get(name) {
  | Some(value) => Some(value.contents)
  | None => t.enclosing->Option.flatMap(get(_, name))
  }

let rec assign = (t, name, value) =>
  switch t.current->Map.String.get(name) {
  | Some(valueRef) =>
    valueRef.contents = value
    Ok()
  | None =>
    switch t.enclosing {
    | None => Error()
    | Some(enclosing) => assign(enclosing, name, value)
    }
  }

let enterBlock = t => {
  current: Map.String.empty,
  enclosing: Some(t),
}

let exitBlock = t =>
  switch t.enclosing {
  | None => failwith("Internal error: exiting without enclosing scope")
  | Some(enclosing) => enclosing
  }
