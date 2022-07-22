type rec t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil
  | VFunction(function)
  | VClass(class)
  | VInstance(instance)
and function = {name: string, arity: int, call: list<t> => t}
and class = {name: string}
and instance = {class: class, fields: MutableMap.String.t<t>}

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
