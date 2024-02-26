/* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 */

/** Yacc-generated parser */

/* Author: Piotr Polesiuk, 2023,2024 */

%token<string> LID UID TLID
%token BR_OPN BR_CLS SBR_OPN SBR_CLS CBR_OPN CBR_CLS
%token ARROW ARROW2 BAR COLON COMMA DOT EQ SEMICOLON2 SLASH
%token KW_AND KW_DATA KW_EFFECT KW_END KW_FINALLY KW_FN KW_HANDLE KW_HANDLER
%token KW_IMPLICIT KW_IN KW_LABEL KW_LET KW_MATCH KW_METHOD KW_OF KW_REC
%token KW_RETURN KW_TYPE KW_WITH
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

/* ========================================================================= */

ty_expr
: ty_expr_app ARROW ty_expr { make (TArrow($1, $3)) }
| ty_expr_app { $1 }
| KW_TYPE   ty_expr { make (TTypeLbl $2)   }
| KW_EFFECT ty_expr { make (TEffectLbl $2) }
;

ty_expr_app
: ty_expr_app ty_expr_simple { make (TApp($1, $2)) }
| ty_expr_simple { $1 }
;

ty_expr_simple
: BR_OPN ty_expr BR_CLS { make (TParen $2) }
| UID        { make (TVar $1) }
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

expr
: def_list1 KW_IN expr  { make (EDefs($1, $3)) }
| KW_FN expr_250_list1 ARROW2 expr { make (EFn($2, $4))   }
| KW_EFFECT expr_250_list SLASH expr ARROW2 expr
    { make (EEffect($2, $4, $6)) }
| KW_HANDLER expr { make (EHandler $2) }
| expr_10 { $1 }
;

expr_10
: expr_10 COLON ty_expr { make (EAnnot($1, $3)) }
| expr_200 { $1 }
;

expr_200
: expr_250 { $1 }
| expr_250 expr_250_list1 { make (EApp($1, $2)) }
;

expr_250
: expr_simple { $1 }
| expr_250 DOT LID { make (EMethod($1, $3)) }
;

expr_250_list1
: expr_250 expr_250_list { $1 :: $2 }
;

expr_250_list
: /* empty */ { [] }
| expr_250 expr_250_list { $1 :: $2 }
;

expr_simple
: LID                { make (EVar $1)      }
| TLID               { make (EImplicit $1) }
| UID                { make (ECtor $1)     }
| UNDERSCORE         { make EWildcard      }
| BR_OPN BR_CLS      { make EUnit          }
| BR_OPN expr BR_CLS { make (EParen $2)    }
| KW_MATCH expr KW_WITH KW_END { make (EMatch($2, [])) }
| KW_MATCH expr KW_WITH bar_opt match_clause_list KW_END
  { make (EMatch($2, $5)) }
| CBR_OPN field_list CBR_CLS { make (ERecord $2) }
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
| name EQ expr         { make (FldNameVal($1, $3))   }
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
: KW_LET expr EQ expr    { make (DLet($2, $4)) }
| KW_IMPLICIT TLID       { make (DImplicit $2) }
| data_def               { make (DData $1)     }
| data_rec data_rec_rest { make (DDataRec ($1 :: $2)) }
| KW_LABEL  expr         { make (DLabel $2) }
| KW_HANDLE expr EQ expr h_clauses      { make (DHandle($2, $4, $5)) }
| KW_HANDLE expr KW_WITH expr h_clauses { make (DHandleWith($2, $4, $5)) }
| KW_METHOD expr EQ expr { make (DMethod($2, $4)) }
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
