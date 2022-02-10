let hadError = ref(false)

let printLoc = (loc: Location.t) => {
  if loc.start == loc.end {
    `${loc.start.line->Int.toString}:${loc.start.col->Int.toString}`
  } else {
    `${loc.start.line->Int.toString}:${loc.start.col->Int.toString} to ${loc.end.line->Int.toString}:${loc.end.col->Int.toString}`
  }
}
let run = source =>
  switch Lexer.scanTokens(source) {
  | Ok(tokens) =>
    switch tokens->List.toArray->Parser.make->Parser.parse {
    | Ok(ast) =>
      switch Interpreter.interpret(ast) {
      | Ok() => ()
      | Error((msg, loc)) => Js.log2(msg, printLoc(loc))
      }
    | Error((msg, loc)) => Js.log2(msg, printLoc(loc))
    }
  | Error(_) => Js.log("LexError")
  }

let runFile = path => {
  let source = Node.Fs.readFileAsUtf8Sync(path)
  run(source)
}

let runPrompt = () => {
  let lineHandler = line => {
    run(line)
    hadError := false
  }

  let () = Readline.onLine(~prompt="> ", lineHandler)
}

let report = (line, where, message) => {
  Js.log(`[line ${Int.toString(line)}] Error ${where}: ${message}`)
  hadError := true
}

let error = (line, message) => report(line, "", message)

let main = () => {
  let args = Node.Process.argv->Array.sliceToEnd(2)

  switch args {
  | [] => runPrompt()
  | [path] => runFile(path)

  | _ =>
    Js.log("Usage: reslox [script]")
    Node.Process.exit(64)
  }
}

main()

