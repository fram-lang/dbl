(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Propositional variables *)

type t

(** Value of a variable *)
type value =
  | True
  | False
  | SameAs of t

(** Create a fresh variable *)
val fresh : unit -> t

(** Get the value of the variable. *)
val value : t -> value

(** Set the value of the variable to some boolean value. *)
val set_bool : t -> bool -> unit

(** Pretty-print variable as S-expression *)
val to_sexpr : t -> SExpr.t

(** Finite sets of propositional variables *)
module Set : Set.S with type elt = t

(** Finite maps from propositional variables *)
module Map : Map.S with type key = t
