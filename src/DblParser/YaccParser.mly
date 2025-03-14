/* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 */

/** Yacc-generated parser */

%token<string> LID UID TLID QLID
%token<string> OP_0 OP_20 OP_30 OP_40 OP_50 OP_60 OP_70 OP_80 OP_90 OP_100
%token<int> NUM
%token<int64> NUM64
%token<string> STR
%token<char> CHR
%token BR_OPN BR_CLS SBR_OPN SBR_CLS CBR_OPN CBR_CLS
%token ARROW ARROW2 BAR COLON COMMA DOT EQ SEMICOLON2 SLASH GT_DOT
%token KW_ABSTR KW_AS KW_DATA KW_EFFECT KW_EFFROW KW_ELSE KW_END KW_EXTERN
%token KW_FINALLY KW_FN KW_HANDLE KW_HANDLER KW_IF KW_IMPLICIT KW_IMPORT
%token KW_IN KW_LABEL KW_LET KW_MATCH KW_METHOD KW_MODULE KW_OF KW_OPEN KW_PUB
%token KW_REC ATTR_OPEN 
%token KW_RETURN KW_THEN KW_TYPE KW_WITH
%token UNDERSCORE
%token EOF

%type<Raw.import list * Raw.program> file
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

(** Make potentially recursive definition *)
let make_def is_rec data =
  let def = make data in
  if is_rec then make (DRec(false, [def])) else def

%}

%%

bar_opt
: /* empty */ { () }
| BAR         { () }
;

/* ========================================================================= */

lid
: LID { make $1 }
;

var_id
: LID                  { VIdVar $1 }
| BR_OPN op BR_CLS     { VIdBOp ($2).data }
| BR_OPN op DOT BR_CLS { VIdUOp ($2).data }
;

name
: KW_LABEL  { NLabel       }
| LID       { NVar $1      }
| QLID      { NOptionalVar $1 }
| TLID      { NImplicit $1 }
| KW_METHOD LID { NMethod $2   }
;

uid_path
: UID              { NPName $1     }
| UID DOT uid_path { NPSel($1, $3) }
;

/* ========================================================================= */
/* OPERATORS */

op_0
: OP_0 { make $1 }
;

op_20
: OP_20 { make $1 }
;

op_30
: OP_30 { make $1  }
| COMMA { make "," }
;

op_30_no_comma
: OP_30 { make $1  }
;

op_40
: OP_40 { make $1 }
;

op_50
: OP_50 { make $1 }
;

op_60
: OP_60 { make $1  }
| EQ    { make "=" }
;


op_70
: OP_70 { make $1 }
;

op_80
: OP_80 { make $1 }
;

op_90
: OP_90 { make $1  }
| SLASH { make "/" }
;

op_100
: OP_100 { make $1 }
;

uop_150
: OP_80 { make $1 }
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
| uid_path  { make (TVar($1, None)) }
| BR_OPN uid_path COLON kind_expr BR_CLS { make (TVar($2, Some $4)) }
| UNDERSCORE { make TWildcard }
| SBR_OPN effect SBR_CLS { make ($2).data }
| CBR_OPN ty_field_list CBR_CLS { make (TRecord $2) }
;

/* ------------------------------------------------------------------------- */

kind_expr
: kind_expr_simple ARROW kind_expr { make (KArrow($1, $3)) }
| kind_expr_simple { $1 }
;

kind_expr_simple
: BR_OPN kind_expr BR_CLS { make ($2).data }
| KW_TYPE { make KType }
| KW_EFFECT { make KEffect }
| KW_EFFROW { make KEffrow }
| UNDERSCORE { make KWildcard }
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
: KW_TYPE ty_expr      { make (FldAnonType $2)       }
| KW_EFFECT            { make FldEffect              }
| KW_EFFECT EQ ty_expr { make (FldEffectVal $3)      }
| UID                  { make (FldType($1, None))    }
| UID COLON kind_expr  { make (FldType($1, Some $3)) }
| UID EQ ty_expr       { make (FldTypeVal($1, $3))   }
| name                 { make (FldName $1)           }
| name COLON ty_expr   { make (FldNameVal($1, $3))   }
;

ty_field_list
: ty_field                     { [ $1 ]   }
| ty_field COMMA ty_field_list { $1 :: $3 }
;

/* ========================================================================= */

ctor_name
: BR_OPN  BR_CLS       { CNUnit   }
| SBR_OPN SBR_CLS      { CNNil    }
| UID                  { CNId  $1 }
| BR_OPN op BR_CLS     { CNBOp ($2).data }
| BR_OPN op DOT BR_CLS { CNUOp ($2).data }
;

ctor_decl
: ctor_name                     { make (CtorDecl($1, [])) }
| ctor_name KW_OF ty_expr_list1 { make (CtorDecl($1, $3)) }
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
| expr_0 { $1 }
;

expr_no_comma
: def_list1 KW_IN expr_no_comma  { make (EDefs($1, $3)) }
| KW_FN expr_250_list1 ARROW2 expr_no_comma { make (EFn($2, $4))   }
| KW_EFFECT expr_250_list effect_resumption_opt ARROW2 expr_no_comma
    { make (EEffect($2, $3, $5)) }
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
: expr_10_open { $1 }
| expr_10_closed { $1 }
;

expr_10_no_comma
: expr_10_no_comma_open { $1 }
| expr_10_no_comma_closed { $1 }
;

expr_10_open
: KW_IF expr KW_THEN expr_10 { make (EIf($2, $4, None)) }
| KW_IF expr KW_THEN expr_10_closed KW_ELSE expr_10_open
  { make (EIf($2, $4, Some $6)) }
;

expr_10_closed
: KW_IF expr KW_THEN expr_10_closed KW_ELSE expr_10_closed
  { make (EIf($2, $4, Some $6)) }
| expr_20 { $1 }
;

expr_10_no_comma_open
: KW_IF expr KW_THEN expr_10_no_comma { make (EIf($2, $4, None)) }
| KW_IF expr KW_THEN expr_10_no_comma_closed KW_ELSE expr_10_no_comma_open
  { make (EIf($2, $4, Some $6)) }
;

expr_10_no_comma_closed
: KW_IF expr KW_THEN expr_10_no_comma_closed KW_ELSE expr_10_no_comma_closed
  { make (EIf($2, $4, Some $6)) }
| expr_20_no_comma { $1 }
;

// exp1 <- exp2
expr_20
: expr_30 op_20 expr_20 { make (EBOp($1, $2, $3)) }
| expr_30 { $1 }
;

expr_20_no_comma
: expr_30_no_comma op_20 expr_20_no_comma { make (EBOp($1, $2, $3)) }
| expr_30_no_comma { $1 }
;

// exp1 , exp2
expr_30
: expr_30 COLON ty_expr { make (EAnnot($1, $3)) }
| expr_30 op_30 expr_40 { make (EBOp($1, $2, $3)) }
| expr_40 { $1 }
;

expr_30_no_comma
: expr_30_no_comma COLON ty_expr { make (EAnnot($1, $3)) }
| expr_30_no_comma op_30_no_comma expr_40 { make (EBOp($1, $2, $3)) }
| expr_40 { $1 }
;

// exp1 || exp2
expr_40
: expr_50 op_40 expr_40 { make (EBOp($1, $2, $3)) }
| expr_50 { $1 }
;


// exp1 && exp2
expr_50
: expr_60 op_50 expr_50 { make (EBOp($1, $2, $3)) }
| expr_60 { $1 }
;


// exp1 | '==' | '<' | '>' | '|' | '&' | '$' | '#' | '?' exp2
expr_60
: expr_60 op_60 expr_70 { make (EBOp($1, $2, $3)) }
| expr_70 { $1 }
;


// exp1 | '@' | ':' | '^' | '.' exp2
expr_70
: expr_80 op_70 expr_70 { make (EBOp($1, $2, $3)) }
| expr_80 { $1 }
;

// exp1 | '+' | '-' | '~' exp2
expr_80
: expr_80 op_80 expr_90 { make (EBOp($1, $2, $3)) }
| expr_90 { $1 }
;

// exp1 | '*' | '/' | '%'  exp2
expr_90
: expr_90 op_90 expr_100 { make (EBOp($1, $2, $3)) }
| expr_100 { $1 }
;

// exp1 ** exp2
expr_100 
: expr_150 op_100 expr_100 { make (EBOp($1, $2, $3)) }
| expr_150 { $1 }
;

expr_150
: uop_150 expr_150 { make (EUOp($1, $2)) }
| expr_200 { $1 }
;

expr_200
: expr_250 { $1 }
| expr_250 expr_250_list1 { make (EApp($1, $2)) }
| expr_200 GT_DOT lid expr_250_list { make (EMethodCall($1, $3, $4)) }
| KW_EXTERN LID { make (EExtern $2) }
| KW_PUB expr_250 { make (EPub $2) }
;

expr_ctor
: UID { make (ECtor $1) }
;

expr_select
: UID DOT expr_ctor   { (NPName $1, $3) }
| UID DOT expr_300    { (NPName $1, $3) }
| UID DOT expr_select { let (p, e) = $3 in (NPSel($1, p), e) }

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
: LID                { make (EVar $1)     }
| TLID               { make (EImplicit $1)}
| UNDERSCORE         { make EWildcard     }
| NUM                { make (ENum $1)     }
| NUM64              { make (ENum64 $1)   }
| STR                { make (EStr $1)     }
| CHR                { make (EChr $1)     }
| BR_OPN BR_CLS      { make EUnit         }
| BR_OPN expr BR_CLS { make (EParen $2)   }
| SBR_OPN SBR_CLS    { make (EList [])    }
| SBR_OPN expr_comma_sep SBR_CLS { make (EList $2)       }
| KW_MATCH expr KW_WITH KW_END   { make (EMatch($2, [])) }
| KW_MATCH expr KW_WITH bar_opt match_clause_list KW_END
  { make (EMatch($2, $5)) }
| KW_HANDLER expr h_clauses KW_END { make (EHandler($2, $3)) }
| CBR_OPN field_list CBR_CLS { make (ERecord $2) }
| BR_OPN op BR_CLS           { make (EBOpID ($2).data)}
| BR_OPN op DOT BR_CLS       { make (EUOpID ($2).data)}
;

expr_comma_sep
: expr_40                      { [ $1 ]   }
| expr_40 COMMA expr_comma_sep { $1 :: $3 }
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
: KW_TYPE ty_expr       { make (FldAnonType $2)       }
| KW_EFFECT             { make FldEffect              }
| KW_EFFECT EQ ty_expr  { make (FldEffectVal $3)      }
| UID                   { make (FldType($1, None))    }
| UID EQ ty_expr        { make (FldTypeVal($1, $3))   }
| name                  { make (FldName $1)           }
| name EQ expr_no_comma { make (FldNameVal($1, $3))   }
| name COLON ty_expr    { make (FldNameAnnot($1, $3)) }
| KW_MODULE UID         { make (FldModule $2)         }
;

field_list
: field                  { [ $1 ]   }
| field COMMA field_list { $1 :: $3 }
;

/* ========================================================================= */

lids
: LID         { [ $1 ]   }
| LID lids    { $1 :: $2 }
;

lids_sep
: /* empty */         { []       }       
| lids COMMA lids_sep { $1 :: $3 }
| lids                { [ $1 ]   }
;

attributes_pub
: /* empty */                       { ([], false) }
| KW_PUB                            { ([], true ) }
| ATTR_OPEN lids_sep CBR_CLS        { ($2, false) }
| ATTR_OPEN lids_sep CBR_CLS KW_PUB { ($2, true ) }
;

data_vis
: /* empty */ { DV_Private  }
| KW_PUB      { DV_Public   }
| KW_ABSTR    { DV_Abstract }
;

pub
: /* empty */ { false }
| KW_PUB      { true  }
;

rec_opt
: /* empty */ { false }
| KW_REC      { true  }
;

def
: attributes_pub KW_LET rec_opt expr_70 EQ expr 
    { let (attr, pub) = $1 in make_def $3 (DLet(attr, pub, $4, $6)) }
| KW_IMPLICIT TLID implicit_ty_args type_annot_opt
    { make (DImplicit($2, $3, $4)) }
| data_vis KW_DATA rec_opt ty_expr EQ bar_opt ctor_decl_list
    { make_def $3  (DData($1, $4, $7)) }
| data_vis KW_DATA rec_opt ty_expr EQ CBR_OPN ty_field_list CBR_CLS
    { make_def $3  (DRecord($1, $4, $7)) }
| pub KW_LABEL rec_opt expr_70 { make_def $3 (DLabel($1, $4)) }
| pub KW_HANDLE rec_opt expr_70 EQ expr h_clauses
    { make_def $3 (DHandle($1, $4, $6, $7)) }
| pub KW_HANDLE rec_opt expr_70 KW_WITH expr
    { make_def $3 (DHandleWith($1, $4, $6)) }
| pub KW_METHOD rec_opt expr_70 EQ expr { make_def $3 (DMethod($1, $4, $6)) }
| pub KW_METHOD KW_FN var_id { make (DMethodFn($1, $4, $4)) }
| pub KW_METHOD KW_FN var_id EQ var_id { make (DMethodFn($1, $4, $6)) }
| pub KW_MODULE rec_opt UID def_list KW_END
    { make_def $3 (DModule($1, $4, $5)) }
| pub KW_REC def_list KW_END { make (DRec($1, $3)) }
| pub KW_OPEN uid_path { make (DOpen($1, $3)) }
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

import_path_rel
: UID                       { ([] , $1)                       }
| UID SLASH import_path_rel { let (p, n) = $3 in ($1 :: p, n) }
;

import_path
: SLASH import_path_rel { let (p, n) = $2 in IPAbsolute(p, n) }
| import_path_rel       { let (p, n) = $1 in IPRelative(p, n) }
;

import
: KW_IMPORT import_path KW_AS UID { make (IImportAs($2, $4)) }
| KW_IMPORT import_path { make (IImportAs($2, Raw.import_path_name $2)) }
| KW_IMPORT KW_OPEN import_path { make (IImportOpen $3) }
;

import_list
: /* empty */        { []       }
| import import_list { $1 :: $2 }
;

/* ========================================================================= */

program
: def_list { make $1 }
;

file
: import_list program EOF { ($1, $2) }
;

repl
: EOF                  { REPL_Exit      }
| expr SEMICOLON2      { REPL_Expr   $1 }
| def_list1 SEMICOLON2 { REPL_Defs   $1 }
| import SEMICOLON2    { REPL_Import $1 }
;
