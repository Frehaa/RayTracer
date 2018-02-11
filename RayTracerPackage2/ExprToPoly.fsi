module ExprToPoly

type expr = ExprParse.expr
val subst: expr -> (string * expr) -> expr

type simpleExpr
val ppSimpleExpr: simpleExpr -> string
val exprToSimpleExpr: expr -> simpleExpr

type poly
val ppPoly: string -> poly -> string
val simpleExprToPoly: simpleExpr -> string -> poly
