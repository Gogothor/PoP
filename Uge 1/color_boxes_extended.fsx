#r "nuget:DIKU.Canvas, 1.0"

open Canvas

type state = int * int


let draw w h (s: state) =

    let C = create w h
    setFillBox C black (200 + fst (s), 200 + snd (s)) (400 + fst (s), 400 + snd (s))
    setLine C lightgrey (0, 0) (200 + fst (s), 200 + snd (s))
    setLine C lightgrey (w, 0) (400 + fst (s), 200 + snd (s))
    setLine C lightgrey (0, h) (200 + fst (s), 400 + snd (s))
    setLine C lightgrey (w, h) (400 + fst (s), 400 + snd (s))
    C

let react (s: state) (k: key) : state option =
    let step = 10

    match getKey k with
    | LeftArrow -> Some((fst (s) - step, snd (s)))
    | RightArrow -> Some((fst (s) + step, snd (s)))
    | DownArrow -> Some((fst (s), snd (s) + step))
    | UpArrow -> Some((fst (s), snd (s) - step))
    | _ -> None



let width = 600
let height = width

do runApp "Canvas" width height draw react (0, 0)
