open System.Threading
open System.Globalization
Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture

let chk (name,t,r) =
  printf "%s %s\n" name (if t = r then "OK" else "FAILED[t="+(string)t+",r="+(string)r+"]")
let chkExn(name,fn_t) =
  printf "%s %s\n" name (try fn_t(); "FAILED [Exn expected]" with _ -> "OK")

let c1 = Colour.mkColour 1.0 1.0 1.0
let c2 = Colour.mkColour 0.0 0.0 0.0 
let c3 = Colour.mkColour 0.0 1.0 2.0
let c1' = Colour.toColor c1
let c2' = Colour.toColor c2
let c3' = Colour.toColor c3

let tests =
  [("Test01",(string)c1,"[1,1,1]");
   ("Test02",(string)(c1*c1),"[1,1,1]");
   ("Test03",(string)(1.0 * c1),"[1,1,1]");
   ("Test04",(string)(c1+c1),"[2,2,2]");
   ("Test05",(string)(c1*c2),"[0,0,0]");
   ("Test06",(string)(2.5*c1),"[2.5,2.5,2.5]");
   ("Test07",(string)(Colour.getR c3),"0");
   ("Test08",(string)(Colour.getG c3),"1");
   ("Test09",(string)(Colour.getB c3),"2");
   ("Test10",(string)(Colour.scale c1 2.5),"[2.5,2.5,2.5]");
   ("Test11",(string)(Colour.scale c2 2.0),"[0,0,0]");
   ("Test12",(string)(Colour.scale c3 3.0),"[0,3,6]");
   ("Test13",(string)(Colour.merge 0.5 c1 c2),"[0.5,0.5,0.5]");
   ("Test14",(string)(Colour.merge 0.25 c1 c3),"[0.25,1,1.75]");
   ("Test15",(string)(c1'),"Color [A=255, R=255, G=255, B=255]");
   ("Test16",(string)(c2'),"Color [A=255, R=0, G=0, B=0]");
   ("Test17",(string)(c3'),"Color [A=255, R=0, G=255, B=255]");
   ("Test18",(string)(Colour.fromColor c1'),"[1,1,1]");
   ("Test19",(string)(Colour.fromColor c2'),"[0,0,0]");
   ("Test20",(string)(Colour.fromColor c3'),"[0,1,1]")
   ]

let testsExn = 
  [("TestExn1",fun () -> ignore (Colour.mkColour -1.0 0.0 0.0));
   ("TestExn2",fun () -> ignore (Colour.mkColour  1.0 -1.0 0.0));
   ("TestExn3",fun () -> ignore (Colour.mkColour  1.0 0.0 -0.5));
   ("TestExn4",fun () -> ignore (Colour.mkColour -1.0 0.0 0.0));
   ("TestExn5",fun () -> ignore (Colour.scale c1 -1.0));
   ("TestExn6",fun () -> ignore (Colour.merge -0.1 c1 c2));
   ("TestExn7",fun () -> ignore (Colour.merge  1.01 c1 c2));
   ("TestExn8",fun () -> ignore (-1.0 * c1))
  ]

let doTest() =
  printf "ColourTest\n"
  List.iter chk tests
  List.iter chkExn testsExn

let _ = doTest()

  
