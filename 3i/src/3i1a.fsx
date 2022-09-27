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

/// <summary>
/// Generates a turtle command tree, but places it randomly in the canvas
/// </summary>
/// <param name="sz">The size of the tree</param>
/// <returns>A list of turtle commands</returns>
let randomTree (sz: int) : Canvas.turtleCmd list =
    let rnd = System.Random()
    let woff = (w / 10) * (rnd.Next 10) - (w / 2)
    let hoff = (h / 10) * (rnd.Next 10) - (h / 2)

    let pre =
        [ PenUp
          Move hoff
          Turn 90
          Move woff
          Turn -90
          PenDown ]

    let post =
        [ PenUp
          Move -hoff
          Turn 90
          Move -woff
          Turn -90 ]

    pre @ tree sz @ post

turtleDraw (w, h) "Tree" (randomTree sz)
