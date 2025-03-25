(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Set of constraints *)

open TypeBase

type t

(** Empty set of constraints *)
val empty : t

(** Add a list of constraints to the set *)
val add_list : t -> constr list -> t

(** Get the list of upper bounds for a given effect variable *)
val upper_bounds : t -> keffect tvar -> effect list
