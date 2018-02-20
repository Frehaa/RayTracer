#r @"C:\Workspace\RayTracer\F#\vector.dll"
#r @"C:\Workspace\RayTracer\F#\point.dll"
#r @"C:\Workspace\RayTracer\F#\colour.dll"

open System.IO
open Vector
open Point
open Colour

type Shape = 
    | Circle of Point * float

type Light = 
    | Directional of Vector * float
    | Point of Point * float
    | Ambient of float

[<EntryPoint>]
let main args =
    if Array.length args < 2 then failwith "Not enough arguments"
    else
    let (width, height) as canvas = (int args.[0], int args.[1])    
    let origin = mkPoint 0.0 0.0 0.0 // Camera origin
    let (vw, vh, d) as view = 
        let ar = float width / float height
        (1. * ar , 1., 1.)    
    let shapes = [Circle (mkPoint 0.0 0.0 100.0, 40.0)]
    let lights = [Ambient 0.05; Directional (mkVector 0.0 -1.0 0.0, 0.7); Point (mkPoint 100.0 -25.0 -10.0, 0.25)]


    let discriminant b a c = b * b - 4.0 * a * c
    let solve b a d = ((-b - (sqrt d))/(2.0 * a), (-b + sqrt d)/(2.0 * a))
    let circleIntersection a b c r = 
        let d = discriminant b a (c - r * r)
        if d < 0. then (infinity, infinity)
        else solve b a d

    let bf = new StreamWriter(new FileStream("img.ppm", FileMode.OpenOrCreate))

    bf.WriteLine "P3"
    bf.WriteLine (System.String.Format ("{0} {1}", width, height))
    bf.WriteLine "255"
    
    let rec sendRay (origin : Point) (target : Point) shapes distance =
        let D = target - origin
        match shapes with 
        | []            -> distance
        | Circle (C, r) :: shapes'  -> 
            let OC = origin - C
            let a = D * D
            let b = 2.0 * OC * D
            let c = OC * OC
            let (x1, x2) = circleIntersection a b c r
            let shortest = min x1 x2
            sendRay origin target shapes' (min distance shortest)
        | _ -> failwith "Unhandled shape"           

    let calcLight P N = 
        let ambientLight = 0.10
        let L = mkVector 0. -1. 0.
        let n_dot_l  = N * L
        if n_dot_l > 0. then
            ambientLight + 0.90 * n_dot_l/((Vector.magnitude N) * (Vector.magnitude L))
        else 
            ambientLight

    let red = mkColour 1.0 0.0 0.0
    let black = mkColour 0.0 0.0 0.0

    let CanvasPoint = mkPoint 0.0 0.0 1.0 
    let V = mkVector 0.0 0.0 1.0
   
    let colourToString c = string (int(Colour.getR c)) + " " + string (int(Colour.getG c)) + " " + string (int(Colour.getB c))

    for y in [-height/2 .. (height / 2) - 1] do
        for x in [-width/2 .. (width / 2) - 1] do        
            let C = mkPoint (float x * vw / float width) (float y * vh / float height) d
            let V = distance origin C
            let t = sendRay origin C shapes infinity
            // let P = origin + V * t
            let P = Point.move origin (t * V)
            let Circle(C, _)::_ = shapes 
            let N = P - C
            let color = (if 1.0 <= t && t <= 10000.0 then red else black)            
            ((calcLight P N) * 255.0 * color) |> colourToString |> bf.WriteLine


    bf.Close ()
    0;;



    // let sendRay (x, y) (tMin, tMax) = 
    //     let OV = make ((float x * vw / float width) , (float y * vh / float height), d)
    //     let OC = origin - circleCenter
    //     let a = OV * OV       
    //     let b = 2.0 * OC * OV
    //     let c = OC * OC
    //     let (x1, x2) = circleIntersection a b c circleRadius
    //     let shortest = min x1 x2
    //     bf.WriteLine (if tMin <= shortest && shortest <= tMax then "255 0 0" else "0 0 0")

    // let draw f (width, height) = 
    //     let rec drawRec = function
    //         | (x, y) when x = width - 1 && y = height - 1 -> f (x, y)
    //         | (x, y) when x = width - 1 ->
    //             f (x, y)
    //             drawRec (0, y + 1)
    //         | (x, y) -> 
    //             f (x, y)
    //             drawRec (x + 1, y)
    //     drawRec (0, 0)



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
