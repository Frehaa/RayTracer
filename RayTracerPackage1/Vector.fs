module Vector

type Vector =
  | V of float * float * float
  override v.ToString() =
    match v with
      V(x,y,z) -> "["+x.ToString()+","+y.ToString()+","+z.ToString()+"]"

let mkVector x y z = V (x, y, z)
let getX (V(x,_,_)) = x
let getY (V(_,y,_)) = y
let getZ (V(_,_,z)) = z
let getCoord (V(x,y,z)) = (x, y, z)
let multScalar a (V(x,y,z)) = V(a * x, a * y, a * z)
let magnitude (V(x, y, z)) = sqrt (x * x + y * y + z * z)
let dotProduct (V(ux, uy, uz)) (V(vx, vy, vz)) = ux * vx + uy * vy + uz * vz
let crossProduct  (V(ux, uy, uz)) (V(vx, vy, vz)) = V (uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)
let normalise (V(x,y,z) as v) = let m = magnitude v in V(x/m, y/m, z/m)
let round (V(x,y,z)) (d:int) = V(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d))

type Vector with
  static member ( ~- ) (V(x,y,z)) = V(-x,-y,-z)
  static member ( + ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux + vx, uy + vy, uz + vz)
  static member ( - ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux - vx, uy - vy, uz - vz)
  static member ( * ) (s, v) = multScalar s v
  static member ( * ) (v1, v2) = dotProduct v1 v2
  static member ( % ) (v1, v2) = crossProduct v1 v2
  static member ( / ) (v, f) = multScalar (1.0/f) v
