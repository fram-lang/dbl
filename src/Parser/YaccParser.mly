/* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 */

/** Yacc-generated parser */

/* Author: Piotr Polesiuk, 2023 */

%token<string> LID
%token BR_OPN BR_CLS
%token ARROW2 EQ
%token KW_FN KW_IN KW_LET
%token EOF

%type<Raw.program> file
%start file

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

expr
: def_list1 KW_IN expr  { make (EDefs($1, $3)) }
| KW_FN LID ARROW2 expr { make (EFn($2, $4))   }
| expr_200              { $1 }
;

expr_200
: expr_200 expr_simple { make (EApp($1, $2)) }
| expr_simple          { $1 }
;

expr_simple
: LID                { make (EVar $1)   }
| BR_OPN BR_CLS      { make EUnit       }
| BR_OPN expr BR_CLS { make (EParen $2) }
;

/* ========================================================================= */

def
: KW_LET LID EQ expr { make (DLet($2, $4)) }
;

def_list
: /* empty */  { [] }
| def def_list { $1 :: $2 }
;

def_list1
: def def_list { $1 :: $2 }
;

/* ========================================================================= */

file
: def_list EOF { make $1 }
;
