type rec t<'a> = {
  current: Map.String.t<'a>,
  enclosing: option<t<'a>>,
}

let empty = {
  current: Map.String.empty,
  enclosing: None,
}

let define = (t, name, value) => {
  enclosing: t.enclosing,
  current: t.current->Map.String.set(name, value),
}

let rec get = (t, name) =>
  switch t.current->Map.String.get(name) {
  | Some(value) => Some(value)
  | None => t.enclosing->Option.flatMap(get(_, name))
  }

let rec assign = (t, name, value) =>
  if Map.String.has(t.current, name) {
    Ok({...t, current: t.current->Map.String.set(name, value)})
  } else {
    switch t.enclosing {
    | None => Error()
    | Some(enclosing) =>
      let maybeNewEnclosing = assign(enclosing, name, value)
      switch maybeNewEnclosing {
      | Ok(newEnclsoing) => Ok({current: t.current, enclosing: Some(newEnclsoing)})
      | Error() => Error()
      }
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
