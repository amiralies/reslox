open Location

type rec t = located<stmt>
and stmt =
  | Print(Expr.t)
  | Expression(Expr.t)

