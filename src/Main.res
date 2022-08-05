type runResult = SyntaxError | RuntimeError | AnalyzeError | Fine

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

let reportAnalyzeError = (line, where, message) => {
  let where = switch where {
  | None => ""
  | Some(where) => `at '${where}'`
  }

  Js.Console.error(`[line ${line->Int.toString}] Error ${where}: ${message}`)
}

let run = source =>
  switch Lexer.scanTokens(source) {
  | Ok(tokens) =>
    let parser = Parser.make(tokens->List.toArray)
    switch parser->Parser.parse {
    | Ok(ast) =>
      switch StaticAnalyzer.analyze(ast) {
      | Ok(_) =>
        switch Interpreter.interpret(ast) {
        | Ok() => Fine
        | Error(msg, loc) =>
          reportRuntimeError(loc.start.line, msg)
          RuntimeError
        }
      | Error((msg, loc, maybeWhere)) =>
        reportAnalyzeError(loc.start.line, maybeWhere, msg)
        AnalyzeError
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
  | SyntaxError | AnalyzeError => Node.Process.exit(65)
  | RuntimeError => Node.Process.exit(70)
  | Fine => Node.Process.exit(0)
  }
}

let runPrompt = () => {
  let lineHandler = line => {
    let _runResult = run(line)
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
