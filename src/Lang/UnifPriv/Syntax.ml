(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Unif Language. *)

open SyntaxNode
open TypeBase

type type_expr = type_expr_data node
and type_expr_data =
  | TE_Type      of typ
  | TE_Effect    of type_expr list
  | TE_PureArrow of scheme_expr * type_expr
  | TE_Arrow     of scheme_expr * type_expr * type_expr
  | TE_Handler   of
    { effect   : tvar;
      cap_type : type_expr;
      in_type  : type_expr;
      in_eff   : type_expr;
      out_type : type_expr;
      out_eff  : type_expr
    }
  | TE_Label of
    { effect    : type_expr;
      delim_tp  : type_expr;
      delim_eff : type_expr
    }
  | TE_App    of type_expr * type_expr
  | TE_Option of type_expr

and scheme_expr = {
  se_pos   : Position.t;
  se_targs : named_tvar list;
  se_named : named_scheme_expr list;
  se_body  : type_expr
}

and named_scheme_expr = name * scheme_expr

type ctor_decl_expr = {
  cde_name        : string;
  cde_targs       : named_tvar list;
  cde_named       : named_scheme_expr list;
  cde_arg_schemes : scheme_expr list
}

(* ========================================================================= *)
type var = Var.t

type data_def =
  | DD_Data of
    { tvar  : tvar;
      proof : var;
      args  : named_tvar list;
      ctors : ctor_decl_expr list;
      strictly_positive : bool
    }
  | DD_Label of
    { tvar      : tvar;
      var       : var;
      delim_tp  : type_expr;
      delim_eff : type_expr
    }

type pattern = pattern_data node
and pattern_data =
  | PWildcard
  | PVar   of var * scheme
  | PCtor  of string * int * expr * tvar list * pattern list * pattern list
  | PAnnot of pattern * scheme_expr

and poly_expr = poly_expr_data node
and poly_expr_data =
  | EOptionPrf
  | EVar     of var
  | EPolyFun of tvar list * (var * scheme) list * expr
  | EHole    of poly_expr option BRef.t

and expr = expr_data node
and expr_data =
  | EUnitPrf
  | EInst       of poly_expr * type_expr list * poly_expr list
  | ENum        of int
  | ENum64      of int64
  | EStr        of string
  | EChr        of char
  | EPureFn     of var * scheme * expr
  | EFn         of var * scheme * expr
  | EAppPoly    of expr * poly_expr
  | EAppMono    of expr * expr
  | ELetPoly    of var * poly_expr * expr
  | ELetMono    of var * expr * expr
  | ELetRec     of rec_def list * expr
  | ECtor       of expr * int * typ list * poly_expr list * poly_expr list
  | EData       of data_def list * expr
  | EMatchEmpty of expr * expr * typ * effect
  | EMatch      of expr * match_clause list * typ * effect
  | EHandle     of tvar * var * expr * expr
  | EHandler    of
    { label     : var;
      effect    : tvar;
      delim_tp  : typ;
      cap_type  : typ;
      cap_body  : expr;
      ret_var   : var;
      body_tp   : typ;
      ret_body  : expr;
      fin_var   : var;
      fin_body  : expr;
    }
  | EEffect     of expr * var * expr * typ
  | EExtern     of string * typ
  | EAnnot      of expr * type_expr * type_expr
  | ERepl       of (unit -> expr) * typ
  | EReplExpr   of expr * expr

and rec_def = var * scheme * poly_expr

and match_clause = pattern * expr

type program = expr
