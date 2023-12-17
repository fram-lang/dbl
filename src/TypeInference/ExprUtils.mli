(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Generalize type to polymorphic scheme. The second parameter is a list of
  implicit parameters. *)
val generalize : Env.t -> (S.name * T.var * T.typ) list ->
  T.expr -> T.typ -> T.expr * T.scheme

(** Instantiate polymorphic expression *)
val instantiate : Env.t -> T.expr -> T.scheme -> T.expr * T.typ
