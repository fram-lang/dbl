(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

type kuvar

type kind

type kind_view =
  | KType
  | KEffect
  | KEffrow
  | KUVar  of kuvar
  | KArrow of kind * kind

(** Kind of all types *)
val k_type : kind

(** Kind of all (closed) effects. Closed effects cannot contain unification
  variables. *)
val k_effect : kind

(** Kind of effect rows. Rows contain at most one unification variable or
  type application. *)
val k_effrow : kind

(** Arrow kind. The result kind (the second parameter) must be non-effect
  kind. *)
val k_arrow : kind -> kind -> kind

(** Create an arrow kind with multiple parameters. *)
val k_arrows : kind list -> kind -> kind

(** Create a fresh unification kind variable *)
val fresh_uvar : ?non_effect:bool -> unit -> kind

(** Reveal a top-most constructor of a kind *)
val view : kind -> kind_view

(** Check if given kind contains given unification variable *)
val contains_uvar : kuvar -> kind -> bool

(** Check if given kind cannot be effect kind, even if it is a unification
  variable. The main purpose of this function is to use it in assert
  statements. *)
val non_effect : kind -> bool

(** Check whether given kind is an effect kind (but not unification
  variable). *)
val is_effect : kind -> bool

(** Add non-effect constraint to given kind. Returns true on success.
  Returns false if given kind is an effect kind. *)
val set_non_effect : kind -> bool

(** Operations on kind unification variables *)
module KUVar : sig
  val equal : kuvar -> kuvar -> bool

  val set : kuvar -> kind -> bool

  val set_safe : kuvar -> kind -> unit
end
