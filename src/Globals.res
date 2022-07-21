open Value

let globals = list{
  (
    "clock",
    VFunction({
      arity: 0,
      toString: "<native fn>",
      call: _ => VNumber((Js.Date.now() /. 1000.0)->Js.Math.floor_float),
    }),
  ),
}
