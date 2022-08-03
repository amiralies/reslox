type rec t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil
  | VFunction(callable)
  | VClass(class)
  | VInstance(instance)
and callable = {name: string, arity: int, call: list<t> => t, toString: string}
and class = {name: string, maybeSuperClass: option<class>, methods: Map.String.t<method>}
and instance = {class: class, fields: MutableMap.String.t<t>}
and method = {name: string, bind: instance => callable}

let printValue = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) =>
    let isNegativeZero: float => bool = %raw(`x => Object.is(x, -0)`)
    if isNegativeZero(f) {
      "-0"
    } else {
      Float.toString(f)
    }
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  | VFunction({toString}) => toString
  | VClass({name}) => name
  | VInstance({class}) => `${class.name} instance`
  }
