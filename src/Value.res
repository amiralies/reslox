type t =
  | VString(string)
  | VNumber(float)
  | VBool(bool)
  | VNil

let print = value =>
  switch value {
  | VString(s) => s
  | VNumber(f) => Float.toString(f)
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  }

