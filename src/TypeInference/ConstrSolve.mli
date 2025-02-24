(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint solving. *)

(** Solve only those constraints that concern concrete types. *)
val solve_partial : Constr.t list -> Constr.t list

(** Try to solve all constraints *)
val solve_all : Constr.t list -> unit
