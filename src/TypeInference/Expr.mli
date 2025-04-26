(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

open Common
open BiDirectional
open TypeCheckFix

(** Infer the type of an expression. When the expression is applied to some
  arguments, the [?app_type] parameter, if provided, specifies the type of
  the application. *)
val infer_expr_type : tcfix:tcfix -> ?app_type:T.typ ->
  'st Env.t -> S.expr -> infer expr_result

(** Check the type of an expression. *)
val check_expr_type : tcfix:tcfix ->
  'st Env.t -> S.expr -> T.typ -> check expr_result
