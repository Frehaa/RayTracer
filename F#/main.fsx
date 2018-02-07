open System.IO
open System






type Pixel = int * int * int
type Vector = float * float
type Shape = Circle of Vector * float | Box of Vector * float * float

[<EntryPoint>]
let main args =
    let bf = new StreamWriter(new FileStream("img.ppm", FileMode.OpenOrCreate))
    let width = 2
    let height = 2

    bf.WriteLine "P3"
    bf.WriteLine (System.String.Format ("{0} {1}", width, height))
    bf.WriteLine "255"

    let writePixel (s : StreamWriter) (r, g, b) = s.WriteLine (System.String.Format("{0} {1} {2}", r, g, b))

    writePixel bf (255, 0, 0)

    bf.WriteLine(System.String.Format ("{0} {1} {2}\t{3} {4} {5}", 255, 0, 0, 0, 255, 0))
    bf.WriteLine(System.String.Format ("{0} {1} {2}\t{3} {4} {5}", 255, 0, 0, 0, 255, 0))

    bf.Close()
    0;;
