(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023,2024 *)

type kuvar

type kind

type kind_view =
  | KType
  | KEffect
  | KClEffect
  | KUVar  of kuvar
  | KArrow of kind * kind

(** Kind of all types *)
val k_type : kind

(** Kind of all effects *)
val k_effect : kind

(** Kind of all simple (closed) effects. These effects cannot contain
  unification variables. *)
val k_cleffect : kind

(** Arrow kind *)
val k_arrow : kind -> kind -> kind

(** Create an arrow kind with multiple parameters. *)
val k_arrows : kind list -> kind -> kind

(** Create a fresh unification kind variable *)
val fresh_uvar : unit -> kind

(** Reveal a top-most constructor of a kind *)
val view : kind -> kind_view

(** Check if given kind contains given unification variable *)
val contains_uvar : kuvar -> kind -> bool

(** Operations on kind unification variables *)
module KUVar : sig
  val equal : kuvar -> kuvar -> bool

  val set : kuvar -> kind -> unit
end
