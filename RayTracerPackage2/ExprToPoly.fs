module ExprToPoly

(*#load "ExprParse.fs"*)

open ExprParse
type expr = ExprParse.expr

let rec ppExpr = function
  | FNum c -> string(c)
  | FVar s -> s
  | FAdd(e1,e2) -> "(" + (ppExpr e1) + " + " + (ppExpr e2) + ")"
  | FMult(e1,e2) -> (ppExpr e1) + " * " + (ppExpr e2)
  | FExponent(e,n) -> "(" + (ppExpr e) + ")^" + string(n)

let rec subst e (x,ex) =
  match e with    
  | FNum c -> FNum c
  | FVar s -> failwith "TO BE IMPLEMENTED"

type atom = ANum of float | AExponent of string * int
type atomGroup = atom list  
type simpleExpr = SE of atomGroup list
let isSimpleExprEmpty (SE ags) = ags = [] || ags = [[]]

let ppAtom = function
  | ANum c -> string(c)
  | AExponent(s,1) -> s
  | AExponent(s,n) -> s+"^"+(string(n))
let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

let rec combine xss = function
  | [] -> []
  | ys::yss -> List.map ((@) ys) xss @ combine xss yss

let rec simplify = function
  | FNum c -> [[ANum c]]
  | FVar s -> [[AExponent(s,1)]]
  | FAdd(e1,e2) -> simplify e1 @ simplify e2
  | FMult(e1,e2) -> failwith "combine ...TO BE IMPLEMENTED"
  | FExponent(e1,0) -> failwith "TO BE IMPLEMENTED"
  | FExponent(e1,1) -> failwith "TO BE IMPLEMENTED"
  | FExponent(e1,n) -> failwith "TO BE IMPLEMENTED"

let simplifyAtomGroup ag = failwith "TO BE IMPLEMENTED"

let simplifySimpleExpr (SE ags) =
  let ags' = List.map simplifyAtomGroup ags
  // Add atom groups with only constants together.
  failwith "TO BE IMPLEMENTED"
  // Last task is to group similar atomGroups into one group.
  failwith "TO BE IMPLEMENTED"

let exprToSimpleExpr e = simplifySimpleExpr (SE (simplify e))

type poly = P of Map<int,simpleExpr>

let ppPoly v (P p) =
  let pp (d,ags) =
    let prefix = if d=0 then "" else ppAtom (AExponent(v,d))
    let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
    prefix + postfix
  String.concat "+" (List.map pp (Map.toList p))

(* Collect atom groups into groups with respect to one variable v *)
let splitAG v m = function
  | [] -> m
  | ag ->
    let eqV = function AExponent(v',_) -> v = v' | _ -> false
    let addMap d ag m = 
      failwith "TO BE IMPLEMENTED"
    match List.tryFind eqV ag with
      | Some (AExponent(_,d)) ->
        let ag' = List.filter (not << eqV) ag
        addMap d ag' m
      | Some _ -> failwith "splitAG: Must never come here! - ANum will not match eqV"
      | None -> addMap 0 ag m

let simpleExprToPoly (SE ags) v =
  P (List.fold (splitAG v) Map.empty ags)

let exprToPoly e v = (exprToSimpleExpr >> simplifySimpleExpr >> simpleExprToPoly) e v


(* Test Examples *)
(*
let plane = FAdd (FMult (FVar "a", FVar "x"), FAdd (FMult (FVar "b", FVar "y"), FAdd (FMult (FVar "c", FVar "z"), FVar "d")))
let planeStr = "ax+by+cz+d"

let ex = FAdd (FVar "ox", FMult (FVar "t", FVar "dx"))
let ey = FAdd (FVar "oy", FMult (FVar "t", FVar "dy"))
let ez = FAdd (FVar "oz", FMult (FVar "t", FVar "dz"))
let planeSubst = List.fold subst plane [("x",ex);("y",ey);("z",ez)] 
let plane_d = exprToPoly planeSubst "t"
let _ = printf "%s\n" (ppPoly "t" plane_d)
(* Correct result for plane (d+c*oz+b*oy+a*ox)+t(c*dz+b*dy+a*dx) *)

let circle = FAdd (FExponent (FVar "x", 2) ,
                   FAdd (FExponent (FVar "y", 2),
                         FAdd (FExponent (FVar "z", 2),
                               FMult (FNum -1.0, FExponent (FVar "r", 2))))) 
let circleStr = "x^2+y^2+z^2+-1r^2"
let circleSubst = List.fold subst circle [("x",ex);("y",ey);("z",ez)]
let circle_d = exprToPoly circleSubst "t"
let _ = printf "%s\n" (ppPoly "t" circle_d)
(* Correct result for circle
     (oz^2 + oy^2 + ox^2 + -r^2) + t(2*dz*oz+2*dy*oy+2*dx*ox) + t^2(dz^2+dy^2+dx^2)
The constant 2 could be optimized out.
*)

let test03 n =
  let e = FExponent (FAdd (FVar "a", FVar "b"), n)
  simplifySimpleExpr (exprToSimpleExpr e)

let _ = for (i:int) in [1..5] do printf "(a + b)^%d = %s\n" i (ppSimpleExpr(test03 i))
(*
Expected results
(a + b)^1 = a+b
(a + b)^2 = 2*a*b+a^2+b^2
(a + b)^3 = 3*a*b^2+3*a^2*b+a^3+b^3
(a + b)^4 = 4*a*b^3+4*a^3*b+6*a^2*b^2+a^4+b^4
(a + b)^5 = 5*a*b^4+5*a^4*b+10*a^2*b^3+10*a^3*b^2+a^5+b^5
*)  


let test04 n =
  let e = FExponent (FAdd (FVar "a", FAdd(FVar "b", FVar "c")), n)
  simplifySimpleExpr (exprToSimpleExpr e)
let _ = for (i:int) in [1..5] do printf "(a + b + c)^%d = %s\n" i (ppSimpleExpr(test04 i))
(*
   Expected Results:
(a + b + c)^1 = a+b+c
(a + b + c)^2 = 2*a*b+2*a*c+2*b*c+a^2+b^2+c^2
(a + b + c)^3 = 3*a*b^2+3*a*c^2+3*a^2*b+3*a^2*c+3*b*c^2+3*b^2*c+6*a*b*c+a^3+b^3+c^3
(a + b + c)^4 = 4*a*b^3+4*a*c^3+4*a^3*b+4*a^3*c+4*b*c^3+4*b^3*c+6*a^2*b^2+6*a^2*c^2+6*b^2*c^2+12*a*b*c^2+12*a*b^2*c+12*a^2*b*c+a^4+b^4+c^4
(a + b + c)^5 = 5*a*b^4+5*a*c^4+5*a^4*b+5*a^4*c+5*b*c^4+5*b^4*c+10*a^2*b^3+10*a^2*c^3+10*a^3*b^2+10*a^3*c^2+10*b^2*c^3+10*b^3*c^2+20*a*b*c^3+20*a*b^3*c+20*a^3*b*c+30*a*b^2*c^2+30*a^2*b*c^2+30*a^2*b^2*c+a^5+b^5+c^5
*)   


(* Example from rayTracerII.tex *)
let sphere = FAdd(FAdd(FAdd(FExponent(FVar "x",2),
                            FExponent(FVar "y",2)),
                       FExponent(FVar "z",2)),
                  FMult(FNum -1.0,FVar "R"))
let ex' = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
let ey' = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
let ez' = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))
let eR = FNum -1.0
let sphereSubst = List.fold subst sphere [("x",ex');("y",ey');("z",ez');("R",eR)]

(* Result
val sphereSubst : expr =
  FAdd
    (FAdd
       (FAdd
          (FExponent (FAdd (FVar "px",FMult (FVar "t",FVar "dx")),2),
           FExponent (FAdd (FVar "py",FMult (FVar "t",FVar "dy")),2)),
        FExponent (FAdd (FVar "pz",FMult (FVar "t",FVar "dz")),2)),
     FMult (FNum -1.0,FNum -1.0))
*)

let sphereSE = simplify sphereSubst

(* Result
val sphereSE : atom list list =
  [[ANum 1.0; AExponent ("px",1); AExponent ("px",1)];
   [ANum 1.0; AExponent ("px",1); AExponent ("dx",1); AExponent ("t",1)];
   [ANum 1.0; AExponent ("dx",1); AExponent ("t",1); AExponent ("px",1)];
   [ANum 1.0; AExponent ("dx",1); AExponent ("t",1); AExponent ("dx",1);
    AExponent ("t",1)]; [ANum 1.0; AExponent ("py",1); AExponent ("py",1)];
   [ANum 1.0; AExponent ("py",1); AExponent ("dy",1); AExponent ("t",1)];
   [ANum 1.0; AExponent ("dy",1); AExponent ("t",1); AExponent ("py",1)];
   [ANum 1.0; AExponent ("dy",1); AExponent ("t",1); AExponent ("dy",1);
    AExponent ("t",1)]; [ANum 1.0; AExponent ("pz",1); AExponent ("pz",1)];
   [ANum 1.0; AExponent ("pz",1); AExponent ("dz",1); AExponent ("t",1)];
   [ANum 1.0; AExponent ("dz",1); AExponent ("t",1); AExponent ("pz",1)];
   [ANum 1.0; AExponent ("dz",1); AExponent ("t",1); AExponent ("dz",1);
    AExponent ("t",1)]; [ANum -1.0; ANum -1.0]]
*)

let _ = ppSimpleExpr (SE sphereSE)

(* Result:
> val it : string =
  "1*px*px+1*px*dx*t+1*dx*t*px+1*dx*t*dx*t+1*py*py+1*py*dy*t+1*dy*t*py+1*dy*t*dy*t+1*pz*pz+1*pz*dz*t+1*dz*t*pz+1*dz*t*dz*t+-1*-1"

*)

(* Simplifying Atom Groups *)
let _ = simplifyAtomGroup [AExponent ("px",1); AExponent ("px",2); ANum -2.0; ANum -2.0]
(* Result
> val it : atom list = [ANum 4.0; AExponent ("px",3)]
*)

(* Simplifying Simple Expressions *)
let _ = simplifySimpleExpr 
          (SE [[ANum 3.0];[ANum 4.0];[AExponent("x",2);AExponent("y",3)];[AExponent("x",2); AExponent("y",3)]])
(* Result
> val it : simpleExpr =
  SE [[ANum 7.0]; [ANum 2.0; AExponent ("x",2); AExponent ("y",3)]]
*)

(*
let m = splitAG "t" Map.empty [ANum 1.0; AExponent ("pz",1); AExponent ("dz",1); AExponent ("t",2)]
let m = splitAG "t" m [ANum 1.0; AExponent ("pz",1); AExponent ("dz",1); AExponent ("t",2)]
*)
(* Result:
> 
val m : Map<int,simpleExpr> =
  map [(2, SE [[ANum 1.0; AExponent ("pz",1); AExponent ("dz",1)]])]

> 
val m : Map<int,simpleExpr> =
  map
    [(2,
      SE
        [[ANum 1.0; AExponent ("pz",1); AExponent ("dz",1)];
         [ANum 1.0; AExponent ("pz",1); AExponent ("dz",1)]])]
*)
*)
