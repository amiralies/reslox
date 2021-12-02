let hadError = ref(false)

let run = source => {
  Js.log(source)
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
