(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scheme-inference for polymorphic expressions and related constructs:
  actual parameters, and explicit instantiations. *)
(*
open Common
open TypeCheckFix

(** Instantiation context of polymorphic expression. The context is a function
  that takes instantiated expression, its type and effect, and returns
  translated expression, its type and the effect. Instantiation contexts are
  important in case of method calls, because the self parameter should be
  computed before instantiation and supplied as a additional parameter, after
  instantiation. *)
type inst_context =
  T.expr -> T.typ -> ret_effect -> T.expr * T.typ * ret_effect

(** Simple instantiation context, that do not interact with types *)
type simple_context = T.expr -> T.expr

(** Infer scheme of a polymorphic expression. The effect of en expression is
  always in the check-mode. It returns a tuple, that contains the context of
  the polymorphic expression (computing polymorphic expression may have some
  effects, that should be performed before explicit instantiation), the
  translated polymorphic expression, its scheme, and the hints for the scheme
  instantiation. *)
val infer_scheme : tcfix:tcfix ->
  Env.t -> S.poly_expr -> T.effrow ->
    inst_context * T.expr * T.scheme * TypeHints.t

(** Check the scheme of an actual parameter of a function *)
val check_actual_arg : tcfix:tcfix ->
  Env.t -> S.expr -> T.scheme -> T.effrow -> T.expr * ret_effect

(** Check explicit instantiations against given list of named parameters (from
  type scheme). It returns context (represented as meta-function) that
  preserves the order of computations, list of checked instantiations, and
  the effect. *)
val check_explicit_insts : tcfix:tcfix ->
  Env.t ->
  T.named_scheme list -> S.inst list -> TypeHints.inst_cache -> T.effrow ->
    simple_context * (T.name * T.expr) list * ret_effect
*)
