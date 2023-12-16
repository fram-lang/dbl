(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kind

type kind_view =
  | KType
  | KEffect
  | KClEffect

(** Kind of all types *)
val k_type : kind

(** Kind of all effects *)
val k_effect : kind

(** Kind of all simple (closed) effects. These effects cannot contain
  unification variables. *)
val k_cleffect : kind

(** Reveal a top-most constructor of a kind *)
val view : kind -> kind_view
