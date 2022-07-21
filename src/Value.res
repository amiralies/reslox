type rec t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil
  | VFunction({toString: string, arity: int, call: list<t> => t})

let printValue = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) => Float.toString(f)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  | VFunction({toString}) => toString
  }
