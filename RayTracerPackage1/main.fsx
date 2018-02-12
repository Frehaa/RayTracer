[<EntryPoint>]
let main args = 
    let v = Vector.mkVector 5. 1. 15.
    let (V(x, y, z)) = v
    printf "%f" (Vector.getX v)
    0;;