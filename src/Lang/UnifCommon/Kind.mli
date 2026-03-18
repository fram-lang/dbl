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

(** Arrow kind with non-effect right-hand-side. Use this function only when
  there is a guarantee that the right-hand-side kind is not an effect kind. *)
val k_noneff_arrow : t -> t -> t

(** Arrow kind. Returns [None] if the right-hand-side kind is an effect kind.
  *)
val k_arrow : t -> t -> t option

(** Create an arrow kind with multiple parameters. Returns [None] if the
  right-hand-side kind is an effect kind (unless the list of parameter kinds
  is empty). *)
val k_arrows : t list -> t -> t option

(** Create an arrow kind with multiple parameters, assuming that the
  right-hand-side kind is not an effect kind. *)
val k_noneff_arrows : t list -> t -> t

(** Create a fresh unification kind variable *)
val fresh_uvar : unit -> t

(** Reveal the top-most constructor of a kind *)
val view : t -> kind_view

(** Check if given kind contains given unification variable *)
val contains_uvar : kuvar -> t -> bool

(** Operations on kind unification variables *)
module KUVar : sig
  val equal : kuvar -> kuvar -> bool

  (** Set the value of a kind unification variable. Returns [true] if
    successful, [false] if it would break non-effect constraints, i.e. if it
    would result in an effect kind on the right-hand-side of some arrow kind.
    *)
  val set : kuvar -> t -> bool
end

(** Pretty-print kind as S-expression *)
val to_sexpr : t -> SExpr.t
