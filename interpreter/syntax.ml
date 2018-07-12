(* syntax.ml *)
(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Mt | And | Or

type tyvar = int
  
type ty =   TyInt | TyBool
          | TyVar of tyvar
	  | TyFun of ty * ty
    
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

let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyFun (ty1, ty2) -> (pp_ty ty1;
                         print_string " -> ";
			 pp_ty ty2)
  | TyVar var -> (print_string "'";
		  let varchar = char_of_int (97 + var) in
		  print_char varchar)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty ty =
  match ty with
    TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyVar tyvar -> MySet.singleton tyvar
  | TyFun (tya, tyb) -> MySet.union (freevar_ty tya) (freevar_ty tyb)
