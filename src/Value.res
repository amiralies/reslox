type rec t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil
  | VCallable({toString: string, closure: Env.t<t>, arity: int, call: (Env.t<t>, list<t>) => t})

let printValue = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) => Float.toString(f)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  | VCallable({toString}) => toString
  }
