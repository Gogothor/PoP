//
// Simple turtle graphics
//
#r "nuget:diku.canvas, 1.0.1"
open Canvas

/// <summary>
/// Turtle commands for generating a fractal tree and return to the origin.
/// </summary>
/// <param sz>The size of the tree</param>
/// <returns>A list of turtle commands</returns>
let rec tree (sz: int) : Canvas.turtleCmd list =
    if sz < 5 then
        [ Move sz; PenUp; Move(-sz); PenDown ]
    else
        [ Move(sz / 3); Turn -30 ]
        @ tree (sz * 2 / 3)
          @ [ Turn 30; Move(sz / 6); Turn 25 ]
            @ tree (sz / 2)
              @ [ Turn -25; Move(sz / 3); Turn 25 ]
                @ tree (sz / 2)
                  @ [ Turn -25; Move(sz / 6) ]
                    @ [ PenUp
                        Move(-sz / 3)
                        Move(-sz / 6)
                        Move(-sz / 3)
                        Move(-sz / 6)
                        PenDown ]

let w = 600
let h = w
let sz = 100

// turtleDraw (w, h) "Tree" (tree sz)




let randomTree (sz: int) : Canvas.turtleCmd list =
    let rnd = System.Random()
    let v = rnd.Next 10
    let woff = (w / 10) * v - (w / 2)
    let hoff = (h / 10) * v - (h / 2)

    let pre =
        [ PenUp
          Turn 90
          Move hoff
          Turn 90
          Move woff
          Turn 180
          PenDown ]

    pre @ tree sz



// let randomTreeone: canvas =
//     let C = create w h
//     let woff = (w / 10) * v - (w / 2)
//     let hoff = (h / 10) * v - (h / 2)
//     setLine C blue (w / 2, h / 2) (woff, hoff)
//     C

// do show randomTree "d"


turtleDraw (w, h) "Tree" (randomTree sz)
