(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Core Language. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type var = Var.t

type expr =
  | EValue    of value
  | ELet      of var * expr * expr
  | ELetPure  of var * expr * expr
  | ELetIrr   of var * expr * expr
  | EApp      of value * value
  | ETApp      : value * 'k typ -> expr
  | EData      : 'a tvar * var * TVar.ex list * ctor_type list * expr -> expr
  | EMatch    of expr * value * match_clause list * ttype * effect
  | EHandle   of keffect tvar * var * expr * h_expr * ttype * effect
  | ERepl     of (unit -> expr) * effect
  | EReplExpr of expr * string * expr

and value =
  | VUnit
  | VVar  of var
  | VFn   of var * ttype * expr
  | VTFun  : 'k tvar * expr -> value
  | VCtor of expr * int * Type.ex list * value list

and match_clause = {
  cl_tvars : TVar.ex list;
  cl_vars  : var list;
  cl_body  : expr
}

and h_expr =
  | HEffect of ttype * ttype * var * var * expr

type program = expr
