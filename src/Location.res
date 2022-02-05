type pos = {
  line: int,
  col: int,
}

type t = {
  start: pos,
  end: pos,
}

type located<'a> = {
  val: 'a,
  loc: t,
}
