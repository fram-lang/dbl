(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the ConE language. *)

open TypeBase

type var = Var.t

type data_def =
  | DD_Data of
    { tvar     : tvar;
      proof    : var;
      args     : named_tvar list;
      ctors    : ctor_decl list;
      positive : bool
    }
  | DD_Label of
    { tvar      : tvar;
      var       : var;
      delim_tp  : typ;
      delim_eff : effct
    }

type expr =
  | EUnitPrf
  | EOptionPrf
  | ENum      of int
  | ENum64    of int64
  | EStr      of string
  | EChr      of char
  | EVar      of var
  | EFn       of var * scheme * expr
  | ETFun     of tvar * expr
  | ECAbs     of constr list * expr
  | EApp      of expr * expr
  | ETApp     of expr * typ
  | ECApp     of expr
  | ELet      of var * expr * expr
  | ELetPure  of var * expr * expr
  | ELetRec   of rec_def list * expr
  | ERecCtx   of expr
  | EData     of data_def list * expr
  | ECtor     of expr * int * typ list * expr list
  | EMatch    of expr * expr * match_clause list * typ * ceffect
  | EShift    of expr * var * expr * typ
  | EReset    of expr * expr * var * expr
  | EExtern   of string * typ
  | ERepl     of (unit -> expr) * typ * ceffect
  | EReplExpr of expr * string * expr

and rec_def =
  { rd_var    : var;
    rd_body   : expr;
    rd_evars  : tvar list;
    rd_constr : constr list;
    rd_scheme : scheme
  }

and match_clause =
  { cl_tvars : tvar list;
    cl_vars  : var list;
    cl_body  : expr
  }

type program = expr
