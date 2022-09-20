let rec readNonZeroValue () =
    let a = string (System.Console.ReadLine())

    match a with
    | "fsharp" ->
        printfn "Fsharp is cool"
        readNonZeroValue ()
    | "quit" -> printfn "Quitting"
    | _ ->
        printfn "I don't know %A" a
        readNonZeroValue ()

printfn "Please enter the name of a programming language:"
let b = readNonZeroValue ()
