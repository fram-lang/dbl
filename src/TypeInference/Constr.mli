(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraints generated during type inference *)

type t

(** Solve only those constraints that concern concrete types. *)
val solve_partial : t list -> t list

(** Try to solve all constraints *)
val solve_all : t list -> unit
