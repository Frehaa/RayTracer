module Vector
type Vector = 
    | V of float * float * float
    static member ( ~- ) (V(x, y, z))                   = V(-x, -y, -z)
    static member ( + ) (V(x1, y1, z1), V(x2, y2, z2))  = V(x1 + x2, y1 + y2, z1 + z2)
    static member ( - ) (V(x1, y1, z1), V(x2, y2, z2))  = V(x1 - x2, y1 - y2, z1 - z2)
    static member ( * ) (a, V(x, y, z))                 = V(a * x, a * y, a * z)
    static member ( * ) (v, a)                          = a * v
    static member ( * ) (V(x1, y1, z1), V(x2, y2, z2))  = x1 * x2 + y1 * y2 + z1 * z2

let make (x, y, z) = V(x, y, z)
let coord (V(x, y, z)) = (x, y, z)
let norm (V(x, y,z )) = sqrt (x * x + y * y + z * z)
let normalize v = v * (1./norm v);;