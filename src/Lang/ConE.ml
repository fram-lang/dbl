(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The ConE (constraints on effects) language: the result of the
  effect-inference phase. *)

include ConEPriv.TypeBase
include ConEPriv.Syntax

type formula = IncrSAT.Formula.t

type subst = ConEPriv.Subst.t

module TVar    = ConEPriv.TVar
module GVar    = ConEPriv.Effct.GVar

module Effct = struct
  include ConEPriv.Effct
  let subst = ConEPriv.Subst.in_effect
end

module CEffect = ConEPriv.CEffect

module Type = struct
  include ConEPriv.TypeBase
  let collect_gvars = ConEPriv.Type.collect_gvars
  let subst = ConEPriv.Subst.in_type
  let to_sexpr = ConEPriv.SExprPrinter.tr_type
end

module Scheme = struct
  let is_monomorphic = ConEPriv.Type.is_monomorphic
  let of_type = ConEPriv.Type.scheme_of_type
  let to_type = ConEPriv.Type.scheme_to_type
  let collect_gvars = ConEPriv.Type.collect_scheme_gvars
  let collect_gvars_p = ConEPriv.Type.collect_scheme_gvars_p
  let subst = ConEPriv.Subst.in_scheme
  let to_sexpr = ConEPriv.SExprPrinter.tr_scheme
end

module CtorDecl = ConEPriv.CtorDecl

module Constr = struct
  let to_sexpr = ConEPriv.SExprPrinter.tr_constr
end

module Subst = ConEPriv.Subst

module BuiltinType = UnifCommon.BuiltinType

let to_sexpr = ConEPriv.SExprPrinter.tr_program
