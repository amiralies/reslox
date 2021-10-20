type interface

type inChannel

@val @scope("process")
external stdin: inChannel = "stdin"

@val @scope("process.stdout")
external stdoutWrite: string => unit = "write"

type interfaceOptions = {input: inChannel}

@module("readline")
external createInterface: interfaceOptions => interface = "createInterface"

@send
external on: (interface, @string [#line(string => unit)]) => interface = "on"

@send
external prompt: interface => unit = "prompt"

// Custom api
let onLine = (~prompt, onLine) => {
  let interface = createInterface({
    input: stdin,
  })
  stdoutWrite(prompt)

  let enhancedOnLine = line => {
    onLine(line)
    stdoutWrite(prompt)
  }

  let _ = interface->on(#line(enhancedOnLine))
}
