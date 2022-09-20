//
// Draw 4 colored boxes, which rotation clockwise and counter-clockwise when the user presses the arrow keys.
//
#r "nuget:DIKU.Canvas, 1.0"

open Canvas

type state = int

let draw w h (s: state) =
    let C = create w h
    setFillBox C black (200 + s, 200) (400 + s, 400)
    setLine C lightgrey (0, 0) (200 + s, 200)
    setLine C lightgrey (w, 0) (400 + s, 200)
    setLine C lightgrey (0, h) (200 + s, 400)
    setLine C lightgrey (w, h) (400 + s, 400)
    C

let react (s: state) (k: key) : state option =
    match getKey k with
    | LeftArrow -> Some(s - 10)
    | RightArrow -> Some(s + 10)
    | _ -> None

let width = 600
let height = width

do runApp "Canvas" width height draw react 0
