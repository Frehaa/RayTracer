module Point

type Vector = Vector.Vector
type Point =
  | P of float * float * float
  override p.ToString() =
    match p with
      P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"

let mkPoint x y z = P(x, y, z)
let getX (P(x,_,_)) = x
let getY (P(_,y,_)) = y
let getZ (P(_,_,z)) = z
let getCoord (P(x,y,z)) = (x, y, z)
let move ...
let distance (P(px,py,pz)) (P(qx,qy,qz)) = ...
let direction p q = Vector.normalise(distance p q)
let round (P(px,py,pz)) (d:int) = ...

type Point with
  static member ( + ) (P(x,y,z),v: Vector) : Point = ...
  static member ( - ) ...
