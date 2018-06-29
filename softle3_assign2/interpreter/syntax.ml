(* syntax.ml *)
(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Mt | And | Or

(*type ty = TyInt | TyBool*)
    
type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *)
  | BinOp of binOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
  | LetExp of id * exp * exp
  | FunExp of id * exp (* 関数 *)
  | AppExp of exp * exp (* 値の代入 *)
  | LetRecExp of id * id * exp * exp

type program = 
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

(*let pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
*)
