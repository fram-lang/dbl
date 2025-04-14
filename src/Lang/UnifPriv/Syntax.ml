(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Unif Language. *)

open SyntaxNode
open TypeBase

type 'a node = {
  pos  : Position.t;
  pp   : PPTree.t;
  data : 'a
}

type type_expr = type_expr_data node
and type_expr_data =
  | TE_Type      of typ
  | TE_Effect    of type_expr list
  | TE_PureArrow of scheme_expr * type_expr
  | TE_Arrow     of scheme_expr * type_expr * type_expr
  | TE_Handler   of
    { eff_var  : tvar;
      cap_type : type_expr;
      in_type  : type_expr;
      in_eff   : type_expr;
      out_type : type_expr;
      out_eff  : type_expr
    }
  | TE_Label of
    { eff       : type_expr;
      delim_tp  : type_expr;
      delim_eff : type_expr
    }
  | TE_App    of type_expr * type_expr
  | TE_Option of type_expr

and scheme_expr = {
  se_pos   : Position.t;
  se_pp    : PPTree.t;
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
    { tvar   : tvar;
      proof  : var;
      args   : named_tvar list;
      ctors  : ctor_decl_expr list;
      eff    : effct
    }
  | DD_Label of
    { tvar      : tvar;
      var       : var;
      delim_tp  : typ;
      annot     : type_expr;
    }

type proof_expr =
  | PE_Unit
  | PE_Bool
  | PE_Option of typ
  | PE_Var    of var * typ list

type pattern = pattern_data node
and pattern_data =
  | PWildcard
  | PAs       of pattern * var
  | PCtor     of
    string * int * proof_expr * tvar list * pattern list * pattern list
  | PAnnot    of pattern * scheme_expr

type poly_expr = poly_expr_data node
and poly_expr_data =
  | EVar     of var
  | ECtor    of named_tvar list * proof_expr * int
  | EPolyFun of named_tvar list * (name * var * scheme_expr) list * expr
  | EGen     of named_tvar list * (name * var * scheme_expr) list * poly_expr

and poly_fun = poly_fun_data node
and poly_fun_data =
  | PF_Fun  of tvar list * var list * expr
  | PF_Hole of poly_fun option BRef.t

and expr = expr_data node
and expr_data =
  | EInst       of poly_expr * type_expr list * poly_fun list
  | ENum        of int
  | ENum64      of int64
  | EStr        of string
  | EChr        of char
  | EFn         of var * scheme_expr * expr * effct
  | EAppPoly    of expr * poly_fun
  | EAppMono    of expr * expr
  | ELetPoly    of var * poly_expr * expr
  | ELetMono    of var * expr * expr
  | ELetRec     of
    { targs : named_tvar list;
      named : (name * var * scheme_expr) list;
      defs  : rec_def list;
      body  : expr
    }
  | ERecCtx     of expr
  | EData       of data_def list * expr
  | EMatchEmpty of proof_expr * expr * typ * effct
  | EMatch      of expr * match_clause list * typ * effct
  | EMatchPoly  of poly_expr * pattern * expr * typ * effct
  | EHandle     of tvar * var * expr * expr
  | EHandler    of
    { label     : var;
      eff_var   : tvar;
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
  | EAnnot      of expr * type_expr
  | ERepl       of (unit -> expr) * typ
  | EReplExpr   of expr * expr

and rec_def =
  { rd_pos      : Position.t;
    rd_pp       : PPTree.t;
    rd_poly_var : var;
    rd_var      : var;
    rd_scheme   : scheme_expr;
    rd_body     : poly_fun;
  }

and match_clause = pattern * expr

type program = expr
