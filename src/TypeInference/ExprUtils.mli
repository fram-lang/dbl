(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Generalize type to polymorphic scheme *)
val generalize : Env.t -> T.expr -> T.typ -> T.expr * T.scheme

(** Instantiate polymorphic expression *)
val instantiate : Env.t -> T.expr -> T.scheme -> T.expr * T.typ
