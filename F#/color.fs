module Color
let clamp (r, g, b) =
    let r' = if r < 0.0 then 0.0 elif r > 255.0 then 255.0 else r
    let g' = if g < 0.0 then 0.0 elif g > 255.0 then 255.0 else g
    let b' = if b < 0.0 then 0.0 elif b > 255.0 then 255.0 else b
    (r', g', b')

type Color = 
    | C of float * float * float
    static member ( + ) (C(r1, g1, b1), C(r2, g2, b2))  = C(clamp (r1 + r2, g1 + g2, b1 + b2))
    static member ( - ) (C(r1, g1, b1), C(r2, g2, b2))  = C(clamp (r1 - r2, g1 - g2, b1 - b2))        
    static member ( * ) (a, C(r, g, b))                 = C(clamp (r * a, g * a, b * a))    
    static member ( * ) (C(r, g, b), a)                 = C(clamp (r * a, g * a, b * a))                
    // static member ( * ) (C(r1, g1, b1), (r2, g2, b2))   = C(clamp (r1 * r2, g1 * g2, b1 * b2))        

let string (C(r, g, b)) = string (int r) + " " + string (int g) + " " + string (int b)
let color (r, g, b) = C (clamp (r, g, b))
// let color (r, g, b) = C(float r, float g, float b)
let black   = C(0., 0., 0.)
let white   = C(255., 255., 255.)
let red     = C(255., 0., 0.)
let green   = C(0., 255., 0.)
let blue    = C(0., 0., 255.)
