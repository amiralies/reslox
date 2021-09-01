type interface

type inChannel

@val @scope("process")
external stdin: inChannel = "stdin"
type interfaceOptions = {
  input: inChannel,
  prompt: string,
}

@module("readline")
external createInterface: interfaceOptions => interface = "createInterface"

@send
external on: (interface, @string [#line(string => unit)]) => interface = "on"

// Custom api
let onLine = (~prompt, onLine) => {
  let _ = createInterface({
    input: stdin,
    prompt: prompt,
  })->on(#line(onLine))
}
