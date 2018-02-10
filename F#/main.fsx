#r @"D:\Workspace\RayTracer\F#\vector.dll"
open Vector
open System.IO

[<EntryPoint>]
let main args =
    let origin = make (0.0, 0.0, 0.0) // Camera origin
    let (width, height) as canvas = (640, 480)    
    let ar = float width / float height
    let (vw, vh, d) as view = (1. * ar , 1., 1.)    
    let circleCenter = make (0.0, 0.0, 100.0)
    let circleRadius = 40.0    

    let discriminant b a c = b * b - 4.0 * a * c
    let solve b a d = ((-b - (sqrt d))/(2.0 * a), (-b + sqrt d)/(2.0 * a))

    let circleIntersection a b c r = 
        let d = discriminant b a (c - r * r)
        if d < 0. then (infinity, infinity)
        else solve b a d

    // printf "solve stuff: %f %f\n" x1 x2

    // let (x, y, z) = coord (origin + OV * x1)
    // printf "x:%f, y:%f, z:%f\n" x y z

    let draw f (width, height) = 
        let rec drawRec = function
            | (x, y) when x = width - 1 && y = height - 1 -> f (x, y)
            | (x, y) when x = width - 1 ->
                f (x, y)
                drawRec (0, y + 1)
            | (x, y) -> 
                f (x, y)
                drawRec (x + 1, y)
        drawRec (0, 0)
       

    let bf = new StreamWriter(new FileStream("img.ppm", FileMode.OpenOrCreate))

    bf.WriteLine "P3"
    bf.WriteLine (System.String.Format ("{0} {1}", width, height))
    bf.WriteLine "255"
    
    let sendRay (x, y) (tMin, tMax) = 
        let OV = make ((float x * vw / float width) , (float y * vh / float height), d)
        let OC = origin - circleCenter
        let a = OV * OV       
        let b = 2.0 * OC * OV
        let c = OC * OC
        let (x1, x2) = circleIntersection a b c circleRadius
        let shortest = min x1 x2
        bf.WriteLine (if tMin <= shortest && shortest <= tMax then "255 0 0" else "0 0 0")

    for y in [-height/2 .. (height / 2) - 1] do
        for x in [-width/2 .. (width / 2) - 1] do        
            sendRay (x, y) (1.0, 1000.0)

    // draw sendRay canvas
    bf.Close ()
    0;;


    // printf "%f" (coord myLine origin D x1)
    // let ((minX, minY), (maxX, maxY)) as box = ((12, 12), (16, 16))

    // let isIn (x, y) ((minX, minY), (maxX, maxY)) = minX <= x && x <= maxX && minY <= y && y <= maxY



    // let red     = "255 0 0"
    // let black   = "0 0 0"
    // let white   = "255 255 255"

    // let rec color (x, y) ((width, height) as size ) box (w : StreamWriter) = 
    //     w.WriteLine (if isIn (x, y) box then red else black)
    //     match x, y with
    //     | x, y when x = width - 1 && y = height - 1 -> ()
    //     | x, y when x = width - 1 ->
    //         color (0, y + 1) size box w
    //     | x, y -> 
    //         color (x + 1, y) size box w

    // color (0, 0) (width, height) box bf


    // let writePixel (s : StreamWriter) (r, g, b) = s.WriteLine (System.String.Format("{0} {1} {2}", r, g, b))

    // writePixel bf (255, 0, 0)

    // bf.WriteLine(System.String.Format ("{0} {1} {2}\t{3} {4} {5}", 255, 0, 0, 0, 255, 0))
    // bf.WriteLine(System.String.Format ("{0} {1} {2}\t{3} {4} {5}", 255, 0, 0, 0, 255, 0))

    // bf.Close()
    // 0;;
