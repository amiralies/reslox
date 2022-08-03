type runResult = SyntaxError | RuntimeError | Fine(Env.t<Value.t>)

let reportRuntimeError = (line, message) => {
  let tobelogged = message ++ "\n[line " ++ Int.toString(line) ++ "]"
  Js.Console.error(tobelogged)
}

let reportParseError = (locatedToken: Location.located<Token.t>, message) => {
  let where = switch locatedToken.val {
  | EOF => "at end"
  | token => `at '${Token.toLexeme(token)}'`
  }

  let line = locatedToken.loc.start.line->Int.toString
  Js.Console.error(`[line ${line}] Error ${where}: ${message}`)
}

let reportLexError = (e: Lexer.lexerError) => {
  Js.Console.error(`[line ${e.line->Int.toString}] Error: ${Lexer.errorToString(e.desc)}`)
}

let run = (~env=?, source) =>
  switch Lexer.scanTokens(source) {
  | Ok(tokens) =>
    let parser = Parser.make(tokens->List.toArray)
    switch parser->Parser.parse {
    | Ok(ast) =>
      switch Interpreter.interpret(~initialEnv=?env, ast) {
      | Ok(env) => Fine(env)
      | Error(msg, loc) =>
        reportRuntimeError(loc.start.line, msg)
        RuntimeError
      }
    | Error(errors) =>
      errors->List.forEach(((msg, located)) => reportParseError(located, msg))
      SyntaxError
    }

  | Error(errors, partialTokens) =>
    errors->List.forEach(reportLexError)

    let parser = Parser.make(partialTokens->List.toArray)
    switch parser->Parser.parse {
    | Ok(_ast) => ()
    | Error(errors) => errors->List.forEach(((msg, located)) => reportParseError(located, msg))
    }

    SyntaxError
  }

let runFile = path => {
  let source = Node.Fs.readFileAsUtf8Sync(path)
  let runResult = run(source)

  switch runResult {
  | SyntaxError => Node.Process.exit(65)
  | RuntimeError => Node.Process.exit(70)
  | Fine(_) => Node.Process.exit(0)
  }
}

let runPrompt = () => {
  let lastEnv = ref(None)
  let lineHandler = line => {
    let runResult = run(~env=?lastEnv.contents, line)
    switch runResult {
    | SyntaxError
    | RuntimeError => ()
    | Fine(env) => lastEnv := Some(env)
    }
  }

  let () = Readline.onLine(~prompt="> ", lineHandler)
}

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
