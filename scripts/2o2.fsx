let rec factorial (n: uint64) : uint64 =
    if n = uint64 (0) then
        uint64 (1)
    else
        n * (factorial (n - uint64 (1)))

printfn "Please Type N:"
let ninput: uint64 = uint64 (System.Console.ReadLine())
printfn "%i" (factorial ninput)
