type rec t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil
  | VCallable({arity: int, call: list<t> => t})

let print = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) => Float.toString(f)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  | VCallable(_) => "[Callable]"
  }
