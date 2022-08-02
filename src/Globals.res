open Value

let globals = list{
  (
    "clock",
    VFunction({
      arity: 0,
      name: "clock",
      call: _ => VNumber((Js.Date.now() /. 1000.0)->Js.Math.floor_float),
      toString: "<native fn>",
    }),
  ),
}
