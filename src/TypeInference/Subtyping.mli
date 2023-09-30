(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking subtyping of types *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Check if one type is a subtype of another.
  It performs some unifications when necessary. *)
val subtype : Env.t -> T.typ -> T.typ -> bool

(** Coerce given type to an arrow.
  It performs some unifications when necessary. *)
val to_arrow : Env.t -> T.typ -> (T.typ * T.typ) option

(** Coerce given type from an arrow.
  It performs some unifications when necessary. *)
val from_arrow : Env.t -> T.typ -> (T.typ * T.typ) option
