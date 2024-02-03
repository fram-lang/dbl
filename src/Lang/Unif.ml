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

module Name = UnifPriv.Name

module Type = struct
  include UnifPriv.TypeBase
  include UnifPriv.Type
  include UnifPriv.TypeWhnf

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

module NamedScheme = struct
  let subst = UnifPriv.Subst.in_named_scheme
end

module CtorDecl = struct
  let subst = UnifPriv.Subst.in_ctor_decl
end

module Subst = UnifPriv.Subst

type var = Var.t

type data_def = {
  dd_tvar  : tvar;
  dd_proof : var;
  dd_args  : named_tvar list;
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
  | EVar        of var
  | EPureFn     of var * scheme * expr
  | EFn         of var * scheme * expr
  | ETFun       of tvar * expr
  | EApp        of expr * expr
  | ETApp       of expr * typ
  | ELet        of var * scheme * expr * expr
  | ECtor       of expr * int * typ list * expr list
  | EData       of data_def list * expr
  | EMatchEmpty of expr * expr * typ * effrow
  | EMatch      of expr * match_clause list * typ * effrow
  | EHandle     of
    { effect_var : tvar;
      cap_var    : var;
      body       : expr;
      capability : expr;
      ret_var    : var;
      ret_body   : expr;
      result_tp  : typ;
      result_eff : effrow }
  | EHandler    of tvar * var * typ * effrow * expr
  | EEffect     of expr * var * expr * typ
  | ERepl       of (unit -> expr) * typ * effrow
  | EReplExpr   of expr * string * expr

and match_clause = pattern * expr

type program = expr
