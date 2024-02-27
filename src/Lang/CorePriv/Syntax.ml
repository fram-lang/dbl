(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Core Language. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type var = Var.t

type data_def = {
  dd_tvar  : TVar.ex;
  dd_proof : var;
  dd_args  : TVar.ex list;
  dd_ctors : ctor_type list
}

type expr =
  | EValue    of value
  | ELet      of var * expr * expr
  | ELetPure  of var * expr * expr
  | ELetIrr   of var * expr * expr
  | EApp      of value * value
  | ETApp      : value * 'k typ -> expr
  | EData     of data_def list * expr
  | EMatch    of expr * value * match_clause list * ttype * effect
  | ELabel    of keffect tvar * var * ttype * effect * expr
  | EShift    of value * var * expr * ttype
  | EReset    of value * expr * var * expr
  | ERepl     of (unit -> expr) * ttype * effect
  | EReplExpr of expr * string * expr

and value =
  | VUnit
  | VNum    of int
  | VStr    of string
  | VVar    of var
  | VFn     of var * ttype * expr
  | VTFun    : 'k tvar * expr -> value
  | VCtor   of expr * int * Type.ex list * value list
  | VExtern of string * ttype

and match_clause = {
  cl_tvars : TVar.ex list;
  cl_vars  : var list;
  cl_body  : expr
}

type program = expr
