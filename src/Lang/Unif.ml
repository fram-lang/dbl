(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference. *)

include SyntaxNode.Export

include UnifPriv.KindBase
include UnifPriv.TypeBase

type subst = UnifPriv.Subst.t

module Kind = struct
  include UnifPriv.KindBase
end

module TVar = UnifPriv.TVar
module Name = UnifPriv.Name

module Effect = UnifPriv.Effect

module Type = struct
  include UnifPriv.TypeBase
  include UnifPriv.Type
  include UnifPriv.TypeWhnf

  let subst = UnifPriv.Subst.in_type
end

module Scheme = struct
  let of_type        = UnifPriv.Type.mono_scheme
  let to_type        = UnifPriv.Type.scheme_to_type
  let is_monomorphic = UnifPriv.Type.scheme_is_monomorphic
  let uvars          = UnifPriv.Type.scheme_uvars
  let collect_uvars  = UnifPriv.Type.collect_scheme_uvars
  let subst          = UnifPriv.Subst.in_scheme
end

module NamedScheme = struct
  let subst = UnifPriv.Subst.in_named_scheme
end

module CtorDecl = struct
  let collect_uvars = UnifPriv.Type.collect_ctor_uvars

  let subst = UnifPriv.Subst.in_ctor_decl

  let find_index cs name = List.find_index (fun c -> c.ctor_name = name) cs

  let is_positive = UnifPriv.Type.ctor_is_positive
end

module Subst = UnifPriv.Subst
module BuiltinType = UnifPriv.BuiltinType

include UnifPriv.Syntax

module TypeExpr = struct
  let to_type = UnifPriv.TypeExpr.to_type
end

module SchemeExpr = struct
  let of_type_expr = UnifPriv.TypeExpr.mono_scheme_expr
  let to_type_expr = UnifPriv.TypeExpr.of_scheme_expr
  let to_scheme    = UnifPriv.TypeExpr.to_scheme
  let subst        = UnifPriv.TypeExpr.subst_in_scheme
end

module NamedSchemeExpr = struct
  let to_named_scheme = UnifPriv.TypeExpr.to_named_scheme
end

module CtorDeclExpr = struct
  let to_ctor_decl = UnifPriv.TypeExpr.to_ctor_decl
end

module ProofExpr = UnifPriv.ProofExpr
module Ren = UnifPriv.Ren
