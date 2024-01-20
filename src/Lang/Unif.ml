(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

include SyntaxNode.Export

include UnifPriv.KindBase
include UnifPriv.TypeBase

type subst = UnifPriv.Subst.t

module Kind = struct
  include UnifPriv.KindBase
end

module TVar  = UnifPriv.TVar
module Scope = UnifPriv.Scope

module Type = struct
  include UnifPriv.TypeBase
  include UnifPriv.Type

  let subst = UnifPriv.Subst.in_type
end

module Effect = UnifPriv.Effect

module Scheme = struct
  let of_type       = UnifPriv.Type.mono_scheme
  let uvars         = UnifPriv.Type.scheme_uvars
  let collect_uvars = UnifPriv.Type.collect_scheme_uvars
  let refresh       = UnifPriv.Type.refresh_scheme
  let subst         = UnifPriv.Subst.in_scheme
end

module CtorDecl = struct
  let subst = UnifPriv.Subst.in_ctor_decl
end

module Subst = UnifPriv.Subst

type var = Var.t

type data_def = {
  dd_tvar  : tvar;
  dd_proof : var;
  dd_args  : tvar list;
  dd_ctors : ctor_decl list
}

type pattern = pattern_data node
and pattern_data =
  | PWildcard
  | PVar  of var * scheme
  | PCtor of string * int * expr * ctor_decl list * tvar list * pattern list

and expr = expr_data node
and expr_data =
  | EUnit
  | EVar      of var
  | EPureFn   of var * scheme * expr
  | EFn       of var * scheme * expr
  | ETFun     of tvar * expr
  | EApp      of expr * expr
  | ETApp     of expr * typ
  | ELet      of var * scheme * expr * expr
  | ECtor     of expr * int * typ list * expr list
  | EData     of data_def list * expr
  | EMatch    of expr * match_clause list * typ * effect
  | EHandle   of tvar * var * expr * h_expr * typ * effect
  | ERepl     of (unit -> expr) * effect
  | EReplExpr of expr * string * expr

and match_clause = pattern * expr

and h_expr = h_expr_data node
and h_expr_data =
  | HEffect of typ * typ * var * var * expr

type program = expr
