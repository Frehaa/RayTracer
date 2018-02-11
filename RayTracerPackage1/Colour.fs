module Colour

exception ColourException
type Colour =
  | RGB of float * float * float
  override rgb.ToString() =
    match rgb with
      RGB(r,g,b) -> "["+r.ToString()+","+g.ToString()+","+b.ToString()+"]"

let mkColour r g b = RGB (r, g, b)
let getR (RGB(r,_,_)) = r
let getG (RGB(_,g,_)) = g
let getB (RGB(_,_,b)) = b
let scale (RGB(r,g,b)) s = ()
let merge w (RGB(r1,g1,b1)) (RGB(r2,g2,b2)) = ()
let toColor (RGB(r,g,b)) = ()
let fromColor (c:System.Drawing.Color) = ()

let clamp (r, g, b) =
    let r' = if r < 0.0 then 0.0 elif r > 255.0 then 255.0 else r
    let g' = if g < 0.0 then 0.0 elif g > 255.0 then 255.0 else g
    let b' = if b < 0.0 then 0.0 elif b > 255.0 then 255.0 else b
    (r', g', b')

type Colour with
  static member ( + ) (RGB(r1,g1,b1),RGB(r2,g2,b2)) = RGB (clamp (r1 + r2, g1 + g2, b1 + b2))
  static member ( * ) (RGB(r1,g1,b1),RGB(r2,g2,b2)) = ...
  static member ( * ) (s,c) = ...
