let hadError = ref(false)
let hadRuntimeError = ref(false)

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

let run = source => {
  let tokens = switch Lexer.scanTokens(source) {
  | Ok(tokens) => tokens
  | Error(errors, partialTokens) =>
    hadError := true
    errors->List.forEach(reportLexError)
    partialTokens
  }

  let parser = tokens->List.toArray->Parser.make
  let maybeAst = Parser.parse(parser)
  switch maybeAst {
  | Ok(ast) =>
    if !hadError.contents {
      let runResult = Interpreter.interpret(ast)
      switch runResult {
      | Ok() => ()
      | Error(msg, loc) =>
        hadRuntimeError := true
        reportRuntimeError(loc.start.line, msg)
      }
    }

  | Error(errors) =>
    hadError := true
    errors->List.forEach(((msg, located)) => reportParseError(located, msg))
  }
}

let runFile = path => {
  let source = Node.Fs.readFileAsUtf8Sync(path)
  run(source)

  if hadError.contents {
    Node.Process.exit(65)
  }

  if hadRuntimeError.contents {
    Node.Process.exit(70)
  }
}

let runPrompt = () => {
  let lineHandler = line => {
    run(line)
    hadError := false
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
