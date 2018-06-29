(* typing.ml *)
open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let rec subst_type sb ty =
  match sb with
  | [] -> ty
  | x :: tl -> (match ty with
      TyInt -> ty
    | TyBool -> ty
    | TyVar var -> if (fst x)=var then subst_type tl (snd x)
                                  else subst_type tl ty
    | TyFun (tya, tyb) ->
       (if TyVar (fst x)=tya then
           (if TyVar (fst x)=tyb
	    then subst_type tl (TyFun ((snd x), (snd x)))
	    else subst_type tl (TyFun ((snd x), tyb)))
        else if TyVar (fst x)=tyb
	     then subst_type tl (TyFun (tya, (snd x)))
	else subst_type tl ty))

let eqs_of_subst s = List.map (fun sx -> (TyVar (fst sx), snd sx)) s

let subst_eqs s eqs = List.map (fun (eqsxf, eqsxs) -> (subst_type s eqsxf, subst_type s eqsxs)) eqs
     
let rec unify l = match l with
    [] -> []
  | (tya, tyb) :: tl ->
     if tya=tyb then unify tl
     else match (tya, tyb) with
       (TyFun(ty11, ty12), TyFun(ty21, ty22)) ->
	 unify ((ty11, ty21) :: (ty12, ty22) :: tl)
     | (TyVar alpha, ty) ->
	if MySet.member alpha (freevar_ty ty) (* ty内にalphaがあるかどうか *)
	  then err ("Type error")
	  else (* ペアの両方にsubst_type α→tyするような関数 *)
	       (alpha, ty) :: unify (subst_eqs [(alpha, ty)] tl)
     | (ty, TyVar alpha) ->
	  if MySet.member alpha (freevar_ty ty)
	  then err ("Type error")
	  else (alpha, ty) :: unify (subst_eqs [(alpha, ty)] tl)
     | _ -> err("Type error")
  
let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | Mt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
(*| _ -> err "Not Implemented!"*)

let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
          Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let (eqs3, ty) = ty_prim op ty1 ty2 in
     let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
     let s3 = unify eqs in
     (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
     let (stest, tytest) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let (s3, ty3) = ty_exp tyenv exp3 in
     (* 制約条件(等式集合)にして、unifyに投げる。解いてもらってsubstにする *)
     let eqs =
       (tytest, TyBool) :: (ty2, ty3) :: (eqs_of_subst stest) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
     let resultsubst = unify eqs in
     (resultsubst, subst_type resultsubst ty2)
  | LetExp (id, exp1, exp2) ->
     let (s1, tyvalue) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp (Environment.extend id tyvalue tyenv) exp2 in
     let resultsubst = unify ((eqs_of_subst s1) @ (eqs_of_subst s2))
     (resultsubst, subst_type resultsubst ty2)
  | FunExp (id, exp) ->
     let domty = TyVar (fresh_tyvar ()) in
     let (s, ranty) = ty_exp (Environment.extend id domty tyenv) exp in
     (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented")
