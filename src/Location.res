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

let dummy = {
  let dummyPos = {line: 0, col: 0}
  {
    start: dummyPos,
    end: dummyPos,
  }
}
