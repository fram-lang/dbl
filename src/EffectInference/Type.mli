(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types and related constructs. *)

open Common

(** Translate a type *)
val tr_type : Env.t -> S.typ -> T.typ

(** Translate a type expression *)
val tr_type_expr : Env.t -> S.type_expr -> T.typ

(** Translate a scheme expression *)
val tr_scheme_expr : Env.t -> S.scheme_expr -> T.scheme

(** Translate a named scheme expression *)
val tr_named_scheme_expr : Env.t -> S.named_scheme_expr -> T.named_scheme
