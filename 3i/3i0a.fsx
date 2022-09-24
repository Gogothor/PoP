#r "nuget:diku.canvas , 1.0.1"
open Canvas

type move = int * int

/// <summary></summary>
/// <param name="lst">List of moves</param>
/// <returns></returns>
let rec fromMoveRec (lst: move list) : Canvas.turtleCmd list =
    match lst with
    | [] -> []
    | elm :: rst -> [ Turn(fst elm); Move(snd elm) ] @ fromMoveRec rst

/// <summary></summary>
/// <param name="lst">List of moves</param>
/// <returns></returns>
let fromMoveMap (lst: move list) : Canvas.turtleCmd list =
    List.concat (List.map (fun elm -> [ Turn(fst elm); Move(snd elm) ]) lst)

/// <summary></summary>
/// <param name="lst">List of moves</param>
/// <returns></returns>
let fromMoveFold (lst: move list) : Canvas.turtleCmd list =
    List.rev (List.fold (fun l m -> Move(snd m) :: Turn(fst m) :: l) [] lst)

/// <summary></summary>
/// <param name="lst">List of moves</param>
/// <returns></returns>
let fromMoveFoldBack (lst: move list) : Canvas.turtleCmd list =
    List.foldBack (fun m l -> Turn(fst m) :: Move(snd m) :: l) lst []







let lst =
    [ move (10, 20)
      move (5, -3)
      move (20, 90) ]

let res = fromMoveRec lst
printfn "%A" res
let res2 = fromMoveMap lst
printfn "%A" res2
let res3 = fromMoveFold lst
printfn "%A" res3
let res4 = fromMoveFoldBack lst
printfn "%A" res4
