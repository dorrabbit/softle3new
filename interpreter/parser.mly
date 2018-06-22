(* parser.mly *)
%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT MT
%token AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token FUN RARROW
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
    (* let x=(expr);; *)
  | LET REC id=ID EQ FUN para=ID RARROW e=Expr SEMISEMI { RecDecl (id, para, e) }
   
Expr :
    e=IfExpr { e }
  | e=LTExpr { e }
  | e=OrExpr { e } 
  | e=LetExpr { e } 
  | e=FunExpr { e }
  | e=LetRecExpr { e }
   
LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | l=PExpr MT r=PExpr { BinOp (Mt, l, r) }
  | e=PExpr { e }

OrExpr :
l=OrExpr OR r=AndExpr { BinOp (Or, l, r) } (* 左結合 *)
  | e=AndExpr { e }

AndExpr :
l=AndExpr AND r=LTExpr { BinOp (And, l, r) } (* &&のほうが||より強い *)
  | e=LTExpr { e }
   
LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }    
   
PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

FunExpr :
    FUN x=ID RARROW e1=Expr { FunExp (x, e1) }
  | e=AppExpr { e }
   
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }
   
AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetRecExpr :
     LET REC id=ID EQ FUN para=ID RARROW e1=Expr IN e2=Expr { LetRecExp (id, para, e1, e2) }
