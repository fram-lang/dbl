(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Core Language. *)

open TypeBase

type var = Var.t

type data_def =
  | DD_Data of
    { tvar              : TVar.ex;
      proof             : var;
      args              : TVar.ex list;
      ctors             : ctor_type list;
      positive : bool
    }
  | DD_Label of
    { tvar      : keffect tvar;
      var       : var;
      tvars     : TVar.ex list;
      val_types : ttype list;
      delim_tp  : ttype;
      delim_eff : effct
    }

type expr =
  | EValue    of value
  | ELet      of var * expr * expr
  | ELetPure  of var * expr * expr
  | ELetIrr   of var * expr * expr
  | ELetRec   of (var * ttype * expr) list * expr
  | ERecCtx   of expr
  | EApp      of value * value
  | ETApp      : value * 'k typ -> expr
  | ECApp     of value
  | EData     of data_def list * expr
  | EMatch    of expr * value * match_clause list * ttype * effct
  | EShift    of value * TVar.ex list * var list * var * expr * ttype
  | EReset    of value * Type.ex list * value list * expr * var * expr
  | ERepl     of (unit -> expr) * ttype * effct
  | EReplExpr of expr * string * expr

and value =
  | VNum    of int
  | VNum64  of int64
  | VStr    of string
  | VVar    of var
  | VFn     of var * ttype * expr
  | VTFun    : 'k tvar * expr -> value
  | VCAbs   of constr list * expr
  | VCtor   of expr * int * Type.ex list * value list
  | VExtern of string * ttype

and match_clause = {
  cl_tvars : TVar.ex list;
  cl_vars  : var list;
  cl_body  : expr
}

type program = expr
