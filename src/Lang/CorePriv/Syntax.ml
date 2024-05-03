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
      ctors : ctor_type list
    }
  | DD_Label of
    { tvar      : keffect tvar;
      var       : var;
      delim_tp  : ttype;
      delim_eff : effect
    }

type expr =
  | EValue    of value
  | ELet      of var * expr * expr
  | ELetPure  of var * expr * expr
  | ELetIrr   of var * expr * expr
  | ELetRec   of (var * ttype * value) list * expr
  | EApp      of value * value
  | ETApp      : value * 'k typ -> expr
  | EData     of data_def list * expr
  | EMatch    of expr * value * match_clause list * ttype * effect
  | EShift    of value * var * expr * ttype
  | EReset    of value * expr * var * expr
  | ERepl     of (unit -> expr) * ttype * effect
  | EReplExpr of expr * string * expr

and value =
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

(** Check if value is productive and can be used as recursive definition.
  The first parameter is a list of variables mutually defined together with
  checked value, and therefore must be guarded by lambda-abstractions *)
let rec is_productive rec_vars v =
  match v with
  | VNum _ | VStr _ | VFn _ | VExtern _ -> true
  | VVar x -> not (List.exists (Var.equal x) rec_vars)

  | VTFun(_, EValue v) -> is_productive rec_vars v
  | VTFun _ -> false

  | VCtor(_, _, _, vs) -> List.for_all (is_productive rec_vars) vs
