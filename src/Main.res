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
