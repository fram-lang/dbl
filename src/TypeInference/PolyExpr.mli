(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scheme-inference for polymorphic expressions *)

open Common
open TypeCheckFix

(** Result of scheme-checking of a polymorphic expression definition. *)
type check_def_result =
  | Mono of check expr_result
    (** The definition is a monomorphic expression. *)

  | Poly of T.poly_expr * Constr.t list
    (** The definition is a polymorphic expression. *)

(** Result of scheme-inference of a polymorphic expression definition. *)
type infer_def_result =
  | PPure of T.poly_expr * T.scheme * Constr.t list
    (** Pure polymorphic expression *)

  | PImpure of infer expr_result
    (** Monomorphic expression. Always impure. *)

(** Instantiation context of polymorphic expression. Instantiation contexts
  are important in case of method calls, because the self parameter should
  be supplied as an additional parameter, after the instantiation. *)
type inst_context

(** Plug an expression into an instantiation context. *)
val plug_inst_context : inst_context -> infer expr_result -> infer expr_result

(** Infer the scheme of a polymorphic expression. When the polymorphic
  expression is applied to some parameters, the [?app_type], if provided,
  specifies the type of the application. The function returns a tuple, that
  contains the context of the polymorphic expression (computing polymorphic
  expression may have some effects, that should be performed before explicit
  instantiation), the translated polymorphic expression, and its scheme. *)
val infer_use_scheme : tcfix:tcfix -> ?app_type:T.typ ->
  Env.t -> S.poly_expr_use -> inst_context * T.poly_expr * T.scheme

(** Check the scheme of a polymorphic expression definition. *)
val check_def_scheme : tcfix:tcfix ->
  Env.t -> S.poly_expr_def -> T.scheme -> check_def_result

(** Infer scheme of a polymorphic expression definition. *)
val infer_def_scheme : tcfix:tcfix ->
  Env.t -> S.poly_expr_def -> infer_def_result
