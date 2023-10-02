(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference. *)

(* Author: Piotr Polesiuk, 2023 *)

include SyntaxNode.Export

include UnifPriv.KindBase
include UnifPriv.TypeBase

type subst = UnifPriv.Subst.t

module Kind = struct
  include UnifPriv.KindBase
end

module TVar = UnifPriv.TVar

module Type = struct
  include UnifPriv.TypeBase
  include UnifPriv.Type

  let subst = UnifPriv.Subst.in_type
end

module Scheme = struct
  let uvars         = UnifPriv.Type.scheme_uvars
  let collect_uvars = UnifPriv.Type.collect_scheme_uvars
end

module Subst = UnifPriv.Subst

type var = Var.t

type expr = expr_data node
and expr_data =
  | EUnit
  | EVar      of var
  | EFn       of var * typ * expr
  | ETFun     of tvar * expr
  | EApp      of expr * expr
  | ETApp     of expr * typ
  | ELet      of var * scheme * expr * expr
  | ERepl     of (unit -> expr)
  | EReplExpr of expr * string * expr

type program = expr
