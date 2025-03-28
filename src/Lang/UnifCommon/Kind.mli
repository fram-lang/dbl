(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

type kuvar

type t

type kind_view =
  | KType
  | KEffect
  | KUVar  of kuvar
  | KArrow of t * t

(** Kind of all types *)
val k_type : t

(** Kind of all effects *)
val k_effect : t

(** Arrow kind *)
val k_arrow : t -> t -> t

(** Create an arrow kind with multiple parameters. *)
val k_arrows : t list -> t -> t

(** Create a fresh unification kind variable *)
val fresh_uvar : unit -> t

(** Reveal a top-most constructor of a kind *)
val view : t -> kind_view

(** Check if given kind contains given unification variable *)
val contains_uvar : kuvar -> t -> bool

(** Operations on kind unification variables *)
module KUVar : sig
  val equal : kuvar -> kuvar -> bool

  val set : kuvar -> t -> unit
end

(** Pretty-print kind as S-expression *)
val to_sexpr : t -> SExpr.t
