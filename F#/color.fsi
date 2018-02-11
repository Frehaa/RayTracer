module Color
[<Sealed>]
type Color = 
    static member ( + ) : Color * Color -> Color
    static member ( - ) : Color * Color -> Color
    static member ( * ) : float * Color -> Color
    static member ( * ) : Color * float -> Color
    // static member ( * ) : Color * Color -> Color

val string : Color -> string
val color : (float * float * float) -> Color
// val color : (int * int * int) -> Color
val black : Color
val white : Color
val red : Color
val green : Color
val blue : Color