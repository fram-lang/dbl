(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the interpreter *)

open Value

type t

(** A box containing a value. The box might be empty in case of a recursive
  definition. *)
type value_box

(** Empty environment *)
val empty : t

(** Extend the environment with a new binding *)
val extend : t -> Var.t -> value -> t

(** Extend the environment with an empty binding *)
val extend_box : t -> Var.t -> t * value_box

(** Update a contents of a box. It is an error to call this function on a
  non-empty box. *)
val update_box : value_box -> value -> unit

(** Lookup a variable in the environment *)
val lookup : t -> Var.t -> value
