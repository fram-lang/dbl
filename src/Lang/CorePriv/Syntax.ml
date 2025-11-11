(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Core Language. *)

open TypeBase

type var = Var.t

type data_def =
  | DD_Data of
    { tvar  : TVar.ex;
      proof : var;
      args  : TVar.ex list;
      ctors : ctor_type list;
      rflag : rflag
    }
  | DD_Label of
    { tvar      : keffect tvar;
      var       : var;
      tvars     : TVar.ex list;
      val_types : ttype list;
      delim_tp  : ttype;
      delim_eff : effct;
      rflag     : rflag
    }

type lit =
  | LNum   of int
  | LNum64 of int64
  | LStr   of string

type expr =
  | EValue    of value
  | ELet      of var * expr * expr
  | ELetPure  of relevance * var * expr * expr
  | ELetRec   of (var * ttype * expr) list * expr
  | ERecCtx   of expr
  | EFn       of var * ttype * expr
  | ETFun      : 'k tvar * expr -> expr
  | ECAbs     of constr list * expr
  | EApp      of expr * value
  | ETApp      : expr * 'k typ -> expr
  | ECApp     of expr
  | EData     of data_def list * expr
  | ECtor     of expr * int * Type.ex list * value list
  | EMatch    of expr * value * match_clause list * ttype * effct
  | EShift    of value * TVar.ex list * var list * var * expr * ttype
  | EReset    of value * Type.ex list * value list * expr * var * expr
  | ERepl     of (unit -> expr) * ttype * effct
  | EReplExpr of expr * string * expr

and value =
  | VLit    of lit
  | VVar    of var
  | VExtern of string * ttype

and match_clause = {
  cl_tvars : TVar.ex list;
  cl_vars  : var list;
  cl_body  : expr
}

type program = expr
