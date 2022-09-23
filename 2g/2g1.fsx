// Simple app
#r "nuget:DIKU.Canvas, 1.0"

open Canvas

type vec = float * float


/// <summary>
/// Given two vectors, add them together
/// </summary>
/// <param name="vec1">First Vector</param>
/// <param name="vec2">Second Vector</param>
/// <returns>New Vector vec1 + vec2</returns>
let add ((a1, b1): vec) ((a2, b2): vec) : vec = a1 + a2, b1 + b2

/// <summary>
///  Given two vectors, multiply them together
/// </summary>
/// <param name="vec1">First Vector</param>
/// <param name="vec2">Second Vector</param>
/// <returns>New Vector vec1 * vec2</returns>
let mul ((a1, b1): vec) (c: float) : vec = a1 * c, b1 * c

/// <summary>
///  Given a vector, rotate it by "r" radians
/// </summary>
/// <param name="vec">Vector</param>
/// <param name="r">Rotation value in Radians</param>
/// <returns>New Vector Rotation(vec)</returns>
let rot ((a, b): vec) (r: float) : vec =
    (a * cos (r) - b * sin (r), a * sin (r) + b * cos (r))

/// <summary>
///  Given a vector, og two float values, convert to integer
/// </summary>
/// <param name="vec">Vector</param>
/// <returns>New Vector in integers</returns>
let toInt ((a, b): vec) : int * int = int (a), int (b)

/// <summary>
///  Given a point and a vector, draw a line from the point to the vector
/// </summary>
/// <param name="c">Canvas</param>
/// <param name="color">Color og the line</param>
/// <param name="v">Vector</param>
/// <param name="p">Point</param>
/// <returns>Unit</returns>
let setVector (c: canvas) (color: color) (v: vec) (p: vec) : unit =
    setLine c color (toInt p) (toInt (add v p))

/// <summary>
///  Draw function for the show function to display
/// </summary>
/// <param name="w">Width of window</param>
/// <param name="h">Height of window</param>
/// <returns>Canvas</returns>
let draw w h =
    let c = create w h
    let centerx = float (w / 2)
    let centery = float (h / 2)

    for i = 1 to 36 do
        setVector c blue (rot (200, 0) ((float (i * 2) * System.Math.PI) / float (36))) (centerx, centery)

    c

let drawMultiple w h =
    let c = create w h
    let centerx = float (w / 2)
    let centery = float (h / 2)
    for i = 1 to 36 do
        setVector c blue (rot (100,0) ((float(i*2)*System.Math.PI)/float(36))) (centerx, centery)
    c



let width = 600
let height = width

do show (drawMultiple width height) "Canvas"
