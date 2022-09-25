#r "nuget:diku.canvas , 1.0.1"
open Canvas

type move = int * int

/// <summary>Recursively turns list of moves to list of turtleCmd commands</summary>
/// <param name="lst">List of moves</param>
/// <returns>List of turtleCmd commands</returns>
let rec fromMoveRec (lst: move list) : Canvas.turtleCmd list =
    match lst with
    | [] -> []
    | elm :: rst -> [ Turn(fst elm); Move(snd elm) ] @ fromMoveRec rst

/// <summary>Uses List.map function to turn list of moves to list of turtleCmd commands</summary>
/// <param name="lst">List of moves</param>
/// <returns>List of turtleCmd commands</returns>
let fromMoveMap (lst: move list) : Canvas.turtleCmd list =
    List.concat (List.map (fun elm -> [ Turn(fst elm); Move(snd elm) ]) lst)

/// <summary>Uses List.fold function to turn list of moves to list of turtleCmd commands</summary>
/// <param name="lst">List of moves</param>
/// <returns>List of turtleCmd commands</returns>
let fromMoveFold (lst: move list) : Canvas.turtleCmd list =
    List.rev (List.fold (fun l m -> Move(snd m) :: Turn(fst m) :: l) [] lst)

/// <summary>Uses List.foldBack function to turn list of moves to list of turtleCmd commands</summary>
/// <param name="lst">List of moves</param>
/// <returns>List of turtleCmd commands</returns>
let fromMoveFoldBack (lst: move list) : Canvas.turtleCmd list =
    List.foldBack (fun m l -> Turn(fst m) :: Move(snd m) :: l) lst []

let lst =
    [ move (10, 30)
      move (-5, 127)
      move (20, 90) ]

let res = fromMoveRec lst
printfn "fromMoveRex: %A" res
let res2 = fromMoveMap lst
printfn "fromMoveMap: %A" res2
let res3 = fromMoveFold lst
printfn "fromMoveFold: %A" res3
let res4 = fromMoveFoldBack lst
printfn "fromMoveFoldBack: %A" res4
