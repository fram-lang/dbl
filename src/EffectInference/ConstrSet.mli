(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Mutable sets of constraints *)

type t

(** Create a new empty set *)
val create : unit -> t

(** Add a constraint to the set *)
val add : t -> Constr.t -> unit

(** Add a list of constraints to the set *)
val add_list : t -> Constr.t list -> unit

(** Convert the set to a list *)
val to_list : t -> Constr.t list

(** Clear the set of constraints *)
val clear : t -> unit
