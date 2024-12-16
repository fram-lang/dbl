(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)
(*
open Common
open TypeCheckFix

(** Infer type of an expression. The effect of an expression is always in
  the check mode. However, pure expressions may returns an information that
  they are pure (see [ret_effect] type). *)
val infer_expr_type : tcfix:tcfix ->
  Env.t -> S.expr -> T.effrow -> T.expr * T.typ * ret_effect

(** Check type and effect of an expression. Returns also information about
  the purity of an expression. *)
val check_expr_type : tcfix:tcfix ->
  Env.t -> S.expr -> T.typ -> T.effrow -> T.expr * ret_effect
*)
