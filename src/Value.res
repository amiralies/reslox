type rec t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil
  | VFunction(callable)
  | VClass(class)
  | VInstance(instance)
and callable = {name: string, arity: int, call: list<t> => t}
and class = {name: string, methods: Map.String.t<method>}
and instance = {class: class, fields: MutableMap.String.t<t>}
and method = {name: string, invoke: instance => callable}

let printValue = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) => Float.toString(f)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  | VFunction({name}) => `[fn: ${name}]`
  | VClass({name}) => `[class: ${name}]`
  | VInstance({class}) => `[instance: of class ${class.name}]`
  }
