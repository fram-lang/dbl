(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Detection of cycles in named parameter resolution. *)

(** Data structure used to detect cycles. *)
type t

(** Empty cycle detection structure. *)
val empty : t

(** Check for cyclic dependencies in named parameters, and update the state
  of the cycle detector. Returns [None] if a cycle is detected, or
  [Some new_state] otherwise. The [size] parameter is used to allow multiple
  usages of the same parameter, as long as the consecutive usages have
  decreasing sizes. If the size is not provided, it defaults to 0. *)
val add_var : t -> ?size:int -> Var.t -> t option
