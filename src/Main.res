let hadError = ref(false)

let run = source =>
  switch Lexer.scanTokens(source) {
  | Ok(tokens) =>
    switch tokens->List.toArray->Parser.make->Parser.parse {
    | Ok(ast) =>
      switch Interpreter.interpret(ast) {
      | Ok(v) => Js.log(Expr.printValue(v))
      | Error((msg, loc)) => Js.log2(msg, loc)
      }
    | Error((msg, loc)) => Js.log2(msg, loc)
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
