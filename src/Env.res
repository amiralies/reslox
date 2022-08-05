let strcmp = String.compare
module StringId = unpack(Id.comparable(~cmp=strcmp))
let globalEnv: MutableMap.t<string, Value.t, _> = MutableMap.make(~id=module(StringId))

type t = list<Map.String.t<ref<Value.t>>>

let empty = list{}

let define = (t: t, name: string, value: Value.t) =>
  switch t {
  | list{} =>
    MutableMap.set(globalEnv, name, value)
    t

  | list{current, ...enclosing} =>
    let current = current->Map.String.set(name, ref(value))
    list{current, ...enclosing}
  }

let rec get = (t, name) =>
  switch t {
  | list{} => globalEnv->MutableMap.get(name)

  | list{current, ...enclosing} =>
    switch current->Map.String.get(name) {
    | Some(value) => Some(value.contents)
    | None => get(enclosing, name)
    }
  }

let rec assign = (t, name: string, value) =>
  switch t {
  | list{} =>
    switch MutableMap.get(globalEnv, name) {
    | None => Error()
    | Some(_) =>
      MutableMap.set(globalEnv, name, value)
      Ok()
    }
  | list{current, ...enclosing} =>
    switch current->Map.String.get(name) {
    | Some(valueRef) =>
      valueRef := value
      Ok()
    | None => assign(enclosing, name, value)
    }
  }

let enterBlock = t => list{Map.String.empty, ...t}

let exitBlock = t =>
  switch t {
  | list{} => failwith("Internal error: exiting without enclosing scope")
  | list{_, ...enclosing} => enclosing
  }
