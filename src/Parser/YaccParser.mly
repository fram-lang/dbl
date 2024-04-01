/* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 */

/** Yacc-generated parser */

%token<string> LID UID TLID
%token<string> OP_0 OP_20 OP_30 OP_40 OP_50 OP_60 OP_70 OP_80 OP_90 OP_100
%token<string> OP_230 
%token<int> NUM
%token<string> STR
%token BR_OPN BR_CLS SBR_OPN SBR_CLS CBR_OPN CBR_CLS
%token ARROW ARROW2 BAR COLON COMMA DOT EQ SEMICOLON2 SLASH SEMICOLON
%token KW_AND KW_DATA KW_EFFECT KW_ELSE KW_END KW_EXTERN KW_FINALLY KW_FN
%token KW_HANDLE KW_HANDLER KW_IF KW_IMPLICIT KW_IN KW_LABEL KW_LET KW_MATCH
%token KW_METHOD KW_MODULE KW_OF KW_OPEN KW_PUB KW_REC KW_RETURN KW_THEN
%token KW_TYPE KW_WITH
%token UNDERSCORE
%token EOF

%type<Raw.program> file
%start file

%type<Raw.repl_cmd> repl
%start repl

%{

open Raw

let current_pos () =
  Position.of_pp
    (Parsing.symbol_start_pos ())
    (Parsing.symbol_end_pos ())

let make data =
  { pos  = current_pos ()
  ; data = data
  }

%}

%%

bar_opt
: /* empty */ { () }
| BAR         { () }
;

/* ========================================================================= */

name
: KW_LABEL { NLabel       }
| LID      { NVar $1      }
| TLID     { NImplicit $1 }
;

uid_path
: UID              { NPName $1     }
| UID DOT uid_path { NPSel($1, $3) }
;

/* ========================================================================= */
/* OPERATORS */

op_0
: OP_0      {  make $1  }
| SEMICOLON {  make ";" }
;

op_20
: OP_20 {  make $1 }
;

op_30
: OP_30 {  make $1  }
| COMMA {  make "," }
;

op_30_no_comma
: OP_30 {  make $1  }
;

op_40
: OP_40 {  make $1 }
;

op_50
: OP_50 {  make $1 }
;

op_60
: OP_60 {  make $1  }
| EQ    { make "=" }
;


op_70
: OP_70 {  make $1 }
;

op_80
: OP_80 {  make $1 }
;

op_90
: OP_90 {  make $1  }
| SLASH {  make "/" }
;

op_100
: OP_100 {  make $1 }
;

uop_150
: OP_80 {  make $1 }
;

uop_230
: OP_230 {  make $1 }
;

op
: op_0   { $1 }
| op_20  { $1 }
| op_30  { $1 }
| op_40  { $1 }
| op_50  { $1 }
| op_60  { $1 }
| op_70  { $1 }
| op_80  { $1 }
| op_90  { $1 }
| op_100 { $1 }
| OP_230 { make $1 }
; 

/* ========================================================================= */

ty_expr
: ty_expr_app ARROW ty_expr { make (TArrow($1, $3)) }
| ty_expr_app { $1 }
;

ty_expr_app
: ty_expr_app ty_expr_simple { make (TApp($1, $2)) }
| KW_TYPE   ty_expr_simple { make (TTypeLbl $2)   }
| KW_EFFECT ty_expr_simple { make (TEffectLbl $2) }
| ty_expr_simple { $1 }
;

ty_expr_simple
: BR_OPN ty_expr BR_CLS { make (TParen $2) }
| uid_path { make (TVar $1) }
| UNDERSCORE { make TWildcard }
| SBR_OPN effect SBR_CLS { make ($2).data }
| CBR_OPN ty_field_list CBR_CLS { make (TRecord $2) }
;

/* ------------------------------------------------------------------------- */

effect
: ty_expr_list                    { make (TEffect($1, None))    }
| ty_expr_list BAR ty_expr_simple { make (TEffect($1, Some $3)) }
;

ty_expr_list
: /* empty */   { [] }
| ty_expr_list1 { $1 }
;

ty_expr_list1
: ty_expr_app                     { [ $1 ]   }
| ty_expr_app COMMA ty_expr_list1 { $1 :: $3 }
;

/* ------------------------------------------------------------------------- */

ty_field
: KW_TYPE ty_expr      { make (FldAnonType $2)     }
| KW_EFFECT            { make FldEffect            }
| KW_EFFECT EQ ty_expr { make (FldEffectVal $3)    }
| UID                  { make (FldType $1)         }
| UID EQ ty_expr       { make (FldTypeVal($1, $3)) }
| name                 { make (FldName $1)         }
| name COLON ty_expr   { make (FldNameVal($1, $3)) }
;

ty_field_list
: ty_field                     { [ $1 ]   }
| ty_field COMMA ty_field_list { $1 :: $3 }
;

/* ========================================================================= */

ctor_decl
: UID                     { make (CtorDecl($1, [])) }
| UID KW_OF ty_expr_list1 { make (CtorDecl($1, $3)) }
;

ctor_decl_list1
: ctor_decl                     { [ $1 ]   }
| ctor_decl BAR ctor_decl_list1 { $1 :: $3 }
;

ctor_decl_list
: /* empty */     { [] }
| ctor_decl_list1 { $1 }
;

/* ========================================================================= */

type_annot_opt
: /* empty */   { None    }
| COLON ty_expr { Some $2 }
;

implicit_ty_args
: /* empty */                   { [] }
| CBR_OPN ty_expr_list1 CBR_CLS { $2 }
;

/* ========================================================================= */

expr
: def_list1 KW_IN expr  { make (EDefs($1, $3)) }
| KW_FN expr_250_list1 ARROW2 expr { make (EFn($2, $4))   }
| KW_EFFECT expr_250_list effect_resumption_opt ARROW2 expr
    { make (EEffect($2, $3, $5)) }
| KW_HANDLER expr { make (EHandler $2) }
| expr_0 { $1 }
;

expr_no_comma
: def_list1 KW_IN expr_no_comma  { make (EDefs($1, $3)) }
| KW_FN expr_250_list1 ARROW2 expr_no_comma { make (EFn($2, $4))   }
| KW_EFFECT expr_250_list effect_resumption_opt ARROW2 expr_no_comma
    { make (EEffect($2, $3, $5)) }
| KW_HANDLER expr_no_comma { make (EHandler $2) }
| expr_0_no_comma { $1 }
;


// exp1 ; exp2
expr_0
: expr_10 op_0 expr {make (EBOp($1, $2, $3))}
| expr_10 { $1 }
;


expr_0_no_comma
: expr_10_no_comma op_0 expr_no_comma {make (EBOp($1, $2, $3))}
| expr_10_no_comma{ $1 }
;

effect_resumption_opt
: /* empty */ { None    }
| SLASH expr  { Some $2 }
;

expr_10
: KW_IF expr KW_THEN expr_10 KW_ELSE expr_10 { make (EIf($2, $4, $6)) }
| expr_20 { $1 }
;

expr_10_no_comma
: KW_IF expr_no_comma KW_THEN expr_10_no_comma KW_ELSE expr_10_no_comma{ make (EIf($2, $4, $6)) }
| expr_20_no_comma { $1 }
;

// exp1 <- exp2
expr_20
: expr_30 op_20 expr_20 { make (EBOp($1,$2,$3)) }
| expr_30 { $1 }
;

expr_20_no_comma
: expr_30_no_comma op_20 expr_20_no_comma { make (EBOp($1,$2,$3)) }
| expr_30_no_comma { $1 }
;

expr_30
: expr_30 COLON ty_expr { make (EAnnot($1, $3)) }
| expr_35 { $1 }
;

expr_30_no_comma
: expr_30_no_comma COLON ty_expr { make (EAnnot($1, $3)) }
| expr_35_no_comma { $1 }
;

expr_35
// exp1 , exp2
:  expr_35 op_30 expr_40  { make (EBOp($1,$2,$3)) }
| expr_40 { $1 }
;

expr_35_no_comma
// exp1 , exp2
:  expr_35_no_comma op_30_no_comma expr_40  { make (EBOp($1,$2,$3)) }
| expr_40 { $1 }
;

expr_40
// exp1 || exp2
: expr_50 op_40 expr_40 { make (EBOp($1,$2,$3)) }
| expr_50 { $1 }
;


expr_50
// exp1 && exp2
: expr_60 op_50 expr_50 { make (EBOp($1,$2,$3)) }
| expr_60 { $1 }
;


expr_60
// exp1 | '==' | '<' | '>' | '|' | '&' | '$' | '#' | '?' exp2
: expr_60 op_60 expr_70 { make (EBOp($1,$2,$3)) }
| expr_70 { $1 }
;


expr_70
// exp1 | '@' | ':' | '^' | '.' exp2
: expr_80 op_70 expr_70 { make (EBOp($1,$2,$3)) }
| expr_80 { $1 }
;

expr_80
// exp1 | '+' | '-' | '~' exp2
: expr_80 op_80 expr_90 { make (EBOp($1,$2,$3)) }
| expr_90 { $1 }
;

expr_90
// exp1 | '*' | '/' | '%'  exp2
: expr_90 op_90 expr_100 { make (EBOp($1,$2,$3)) }
| expr_100 { $1 }
;

expr_100 
// exp1 ** exp2
: expr_150 op_100 expr_100 { make (EBOp($1,$2,$3)) }
| expr_150 { $1 }
;

expr_150
: uop_150 expr_150 { make (EUOp($1,$2)) }
| expr_200 { $1 }
;

expr_200
: expr_230 { $1 }
| expr_250 expr_250_list1 { make (EApp($1, $2)) }
| KW_EXTERN LID { make (EExtern $2) }
;

expr_ctor
: UID { make (ECtor $1) }
;

expr_select
: UID DOT expr_ctor   { (NPName $1, $3) }
| UID DOT expr_300    { (NPName $1, $3) }
| UID DOT expr_select { let (p, e) = $3 in (NPSel($1, p), e) }

expr_230
: uop_230 expr_230 { make (EUOp($1,$2))}
| expr_250 { $1 }
;

expr_250
: expr_300    { $1 }
| expr_ctor   { $1 }
| expr_select { let (p, e) = $1 in make (ESelect(p, e)) }
;

expr_250_list1
: expr_250 expr_250_list { $1 :: $2 }
;

expr_250_list
: /* empty */ { [] }
| expr_250 expr_250_list { $1 :: $2 }
;

expr_300
: expr_simple { $1 }
| expr_300 DOT LID { make (EMethod($1, $3)) }
;

expr_simple
: LID                          { make (EVar $1)       }
| TLID                         { make (EImplicit $1)  }
| UNDERSCORE                   { make EWildcard       }
| NUM                          { make (ENum $1)       }
| STR                          { make (EStr $1)       }
| BR_OPN BR_CLS                { make EUnit           }
| BR_OPN expr BR_CLS           { make (EParen $2)     }
| KW_MATCH expr KW_WITH KW_END { make (EMatch($2, []))}
| KW_MATCH expr KW_WITH bar_opt match_clause_list KW_END
  { make (EMatch($2, $5)) }
| CBR_OPN field_list CBR_CLS { make (ERecord $2) }
| BR_OPN op BR_CLS           { make (EBOpID ($2).data)}
| BR_OPN op DOT BR_CLS       { make (EUOpID ($2).data)}

;

/* ========================================================================= */

match_clause
: expr ARROW2 expr { make (Clause($1, $3)) }
;

match_clause_list
: match_clause                       { [ $1 ]   }
| match_clause BAR match_clause_list { $1 :: $3 }
;

/* ========================================================================= */

field
: KW_TYPE ty_expr      { make (FldAnonType $2)       }
| KW_EFFECT            { make FldEffect              }
| KW_EFFECT EQ ty_expr { make (FldEffectVal $3)      }
| UID                  { make (FldType $1)           }
| UID EQ ty_expr       { make (FldTypeVal($1, $3))   }
| name                 { make (FldName $1)           }
| name EQ expr_no_comma { make (FldNameVal($1, $3))   }
| name COLON ty_expr   { make (FldNameAnnot($1, $3)) }
;

field_list
: field                  { [ $1 ]   }
| field COMMA field_list { $1 :: $3 }
;

/* ========================================================================= */

data_def
: KW_DATA ty_expr EQ bar_opt ctor_decl_list
  { make (DD_Data($2, $5)) }
;

data_rec
: KW_DATA KW_REC ty_expr EQ bar_opt ctor_decl_list
  { make (DD_Data($3, $6)) }
;

data_rec_rest
: /* empty */                   { []       }
| KW_AND data_def data_rec_rest { $2 :: $3 }
;

/* ========================================================================= */

def
: KW_IMPLICIT TLID implicit_ty_args type_annot_opt
    { make (DImplicit($2, $3, $4)) }
| KW_PUB def_10 { make (DPub $2) }
| KW_METHOD expr_70 EQ expr { make (DMethod($2, $4)) }
| def_10 { $1 }
;

def_10
: data_def               { make (DData $1)     }
| KW_LET expr_70 EQ expr    { make (DLet($2, $4)) }
| data_rec data_rec_rest { make (DDataRec ($1 :: $2)) }
| KW_LABEL  expr         { make (DLabel $2) }
| KW_HANDLE expr_70 EQ expr h_clauses      { make (DHandle($2, $4, $5)) }
| KW_HANDLE expr_70 KW_WITH expr h_clauses { make (DHandleWith($2, $4, $5)) }
| KW_MODULE UID def_list KW_END { make (DModule($2, $3)) }
| KW_OPEN uid_path { make (DOpen $2) }
;

def_list
: /* empty */  { [] }
| def def_list { $1 :: $2 }
;

def_list1
: def def_list { $1 :: $2 }
;

/* ========================================================================= */

h_clause
: KW_RETURN  expr ARROW2 expr { make (HCReturn($2, $4))  }
| KW_FINALLY expr ARROW2 expr { make (HCFinally($2, $4)) }
;

h_clauses
: /* empty */        { []       }
| h_clause h_clauses { $1 :: $2 }
;

/* ========================================================================= */

file
: def_list EOF { make $1 }
;

repl
: EOF             { REPL_Exit    }
| expr SEMICOLON2 { REPL_Expr $1 }
| def SEMICOLON2  { REPL_Def  $1 }
;
