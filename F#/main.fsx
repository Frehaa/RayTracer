#r @"D:\Workspace\RayTracer\F#\vector.dll"
open System
#r @"D:\Workspace\RayTracer\F#\Point.dll"
#r @"D:\Workspace\RayTracer\F#\Colour.dll"

open System.IO
open Vector
open Point
open Colour

type Shape = 
    | Circle of Point * float
    | Plane of Vector * float * float

// For calculating circle intersection
let discriminant b a c = b * b - 4.0 * a * c
let solve b a d = ((-b - (sqrt d))/(2.0 * a), (-b + sqrt d)/(2.0 * a))
let circleIntersection a b c r = 
    let d = discriminant b a (c - r * r)
    if d < 0. then (infinity, infinity)
    else solve b a d

type Intensity = float

type Light = 
    | Directional of Vector * Intensity
    | Point of Point * Intensity
    | Ambient of Intensity

[<EntryPoint>]
let main args =
    if Array.length args < 2 then failwith "Not enough arguments"
    else
    // Setup environment
    let (width, height) as canvas = (int args.[0], int args.[1])
    let origin = mkPoint 0.0 0.0 0.0 // Camera origin
    let (vw, vh, d) as viewport = 
        let ar = float width / float height
        (1. * ar , 1., 1.)    
    let shapes = [Circle (mkPoint 0.0 0.0 100.0, 40.0)]
    let lights = [Directional (mkVector 0.0 1.0 0.0, 0.7); ] // Ambient 0.05;  Point (mkPoint 100.0 -25.0 -10.0, 0.25)]
   
    // For circles we want to solve t^2(D * D) + t(2(OC * D)) + OC * OC - r^2 = 0
    // Where     
    //  O = camera origin
    //  V = point in the viewport
    //  C = circle center
    //  OC = O - C
    //  D = V - O    

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


    let calcDiffuse n l i = 
        let nl = Vector.dotProduct n l
        if nl > 0. then
            i * nl / (Vector.magnitude n * Vector.magnitude l)
        else 
            0.
        
    let rec calcLight' p n a = function
        | [] -> a
        | Ambient(i):: ls -> calcLight' p n (a + i) ls
        | Directional(d, i)::ls-> calcLight' p n (a + calcDiffuse n d i) ls
        | Point(c, i):: ls -> 
            let d = n - p
            calcLight' p n (a + calcDiffuse n d i) ls

    let calcLight point normal lights = calcLight' point normal 0.0 lights

    let red = mkColour 1.0 0.0 0.0
    let black = mkColour 0.0 0.0 0.0
       
    let colourToString c =
        let c = Colour.scale c 255.0
        string (int(Colour.getR c)) + " " + string (int(Colour.getG c)) + " " + string (int(Colour.getB c))

    let pointToVector p = let (x, y, z) = Point.getCoord p in Vector.mkVector x y z        

    // Writing to ppm
    let bf = new StreamWriter(new FileStream("img.ppm", FileMode.OpenOrCreate))
    bf.WriteLine "P3"
    bf.WriteLine (System.String.Format ("{0} {1}", width, height))
    bf.WriteLine "255"

    for y in [-height/2 .. (height / 2) - 1] do
        for x in [-width/2 .. (width / 2) - 1] do        
            let viewPoint = mkPoint (float x * vw / float width) (float y * vh / float height) d
            let V = distance origin viewPoint
            let t = sendRay origin viewPoint shapes infinity
            
            let P = Point.move origin (t * V)
            let Circle(C, _)::_ = shapes 
            let N = P - C
            let color = (if 1.0 <= t && t <= 10000.0 then red else black)            
            ((calcLight (pointToVector P) N lights) * color) |> colourToString |> bf.WriteLine

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
