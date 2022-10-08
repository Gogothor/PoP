type pos = int * int

/// <summary>dist calculate the distance between 2 points</summary>
/// <param p1>First position</param>
/// <param p2>Second position</param>
/// <returns>The calculated distance between 2 given points</returns>
let dist (p1: pos) (p2: pos) : int =
    let (x1, y1) = p1
    let (x2, y2) = p2
    (pown (x2 - x1) 2 + pown (y2 - y1) 2)

/// <summary>candidates brings the robot closer to the target position</summary>
/// <param src>Source position</param>
/// <param tg>The target position</param>
/// <returns>Distance closer to target position</returns>1
let candidates (src: pos) (tg: pos) : pos list =
    let (x, y) = src
    let cand =
        [ (x + 1, y)
          (x - 1, y)
          (x, y + 1)
          (x, y - 1)
          (x + 1, y + 1)
          (x + 1, y - 1)
          (x - 1, y + 1)
          (x - 1, y - 1) ]
    List.filter (fun (p: pos) -> dist src tg > dist p tg) cand

/// <summary>routes calculates the shortest possible route from src position to tg position </summary>
/// <param src>Source position </param>
/// <param tg>Target position </param>
/// <returns>Shortest possible route to target from source</returns> 
let rec routes (src: pos) (tg: pos) : pos list list =
    if src = tg then
        [ [ src ] ]
    else
        let c = candidates src tg
        if c.Length = 1 || c.Length = 0 then
            c
        else
            let distances = List.map (fun e -> dist e tg) c
            let max = List.max distances
            List.filter (fun e -> dist e tg <> max) c
        |> List.map (fun (elem: pos) -> routes elem tg)
        |> List.collect id
        |> List.map (fun (elem: pos list) -> src :: elem)
printfn "Shortest possible route %A " (routes(3,4)(1,1))
