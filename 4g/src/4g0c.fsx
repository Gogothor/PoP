type pos = int * int
open System.Collections.Generic

let dist (p1: pos) (p2: pos) : int =
    let (x1, y1) = p1
    let (x2, y2) = p2
    (pown (x2 - x1) 2 + pown (y2 - y1) 2)
printfn "%A" (dist(1,3)(2,3))

let candidates (src: pos) (tg: pos) : pos list =
    let (x, y) = src

    let cand =
        [ (x + 1, y)
          (x - 1, y)
          (x, y + 1)
          (x, y - 1) ]
    List.filter (fun (p: pos) -> dist src tg >= dist p tg) cand
printfn "%A" (candidates(3,3)(2,2))

let rec routes (src: pos) (tg: pos) : pos list list =
    if src = tg then
        [ [ src ] ]
    else
        candidates src tg
        |> List.map (fun (elem: pos) -> routes elem tg)
        |> List.collect id
        |> List.map (fun (elem: pos list) -> src :: elem)
printfn "%A" (routes(3,3)(1,1))
