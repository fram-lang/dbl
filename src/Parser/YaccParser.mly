/* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 */

/** Yacc-generated parser */

/* Author: Piotr Polesiuk, 2023 */

%token<string> LID UID TLID
%token BR_OPN BR_CLS SBR_OPN SBR_CLS
%token ARROW ARROW2 BAR COMMA EQ SEMICOLON2 SLASH
%token KW_DATA KW_EFFECT KW_FN KW_HANDLE KW_IMPLICIT KW_IN KW_LET KW_OF
%token KW_WITH
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

ty_expr
: ty_expr_simple ARROW ty_expr { make (TPureArrow($1, $3)) }
| ty_expr_simple ARROW SBR_OPN effect SBR_CLS ty_expr
    { make (TArrow($1, $6, $4)) }
| ty_expr_simple { $1 }
;

ty_expr_simple
: BR_OPN ty_expr BR_CLS { make ($2).data }
| UID        { make (TVar $1) }
| UNDERSCORE { make TWildcard }
;

effect
: ty_expr_list                    { make (TEffect($1, None))    }
| ty_expr_list BAR ty_expr_simple { make (TEffect($1, Some $3)) }
;

ty_expr_list
: /* empty */   { [] }
| ty_expr_list1 { $1 }
;

ty_expr_list1
: ty_expr_simple                     { [ $1 ]   }
| ty_expr_simple COMMA ty_expr_list1 { $1 :: $3 }
;

/* ========================================================================= */

pattern
: LID        { make (PVar $1)  }
| TLID       { make (PName $1) }
| UNDERSCORE { make PWildcard  }
;

/* ========================================================================= */

expr
: def_list1 KW_IN expr  { make (EDefs($1, $3)) }
| KW_FN LID ARROW2 expr { make (EFn($2, $4))   }
| KW_HANDLE LID KW_IN expr KW_WITH h_expr { make (EHandle($2, $4, $6)) }
| expr_200 { $1 }
;

expr_200
: expr_200 expr_simple { make (EApp($1, $2)) }
| expr_simple          { $1 }
;

expr_simple
: LID                { make (EVar $1)   }
| TLID               { make (EName $1)  }
| BR_OPN BR_CLS      { make EUnit       }
| BR_OPN expr BR_CLS { make (EParen $2) }
;

/* ========================================================================= */

h_expr
: KW_EFFECT LID SLASH LID ARROW2 expr { make (HEffect($2, $4, $6)) }
;

/* ========================================================================= */

def
: KW_LET pattern EQ expr { make (DLet($2, $4)) }
| KW_IMPLICIT TLID       { make (DImplicit $2) }
| KW_DATA UID EQ bar_opt ctor_decl_list { make (DData($2, $5)) }
;

def_list
: /* empty */  { [] }
| def def_list { $1 :: $2 }
;

def_list1
: def def_list { $1 :: $2 }
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

file
: def_list EOF { make $1 }
;

repl
: EOF             { REPL_Exit    }
| expr SEMICOLON2 { REPL_Expr $1 }
| def SEMICOLON2  { REPL_Def  $1 }
;
