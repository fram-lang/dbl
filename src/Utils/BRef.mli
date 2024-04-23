(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Backtrackable references *)

type 'a t

(** Create backtrackable reference. *)
val create : 'a -> 'a t 

(** Check physical equality. *)
val equal : 'a t -> 'a t -> bool

(** Get current value. *)
val get : 'a t -> 'a

(** Set a new current value. If `set` is used inside a bracket,
  the old value will be saved for potential backtracking 
  unless it was set within the same bracket. *)
val set : 'a t -> 'a -> unit

(** Run computation and commit changes on success and
  backtrack on failure. *)
val bracket : (unit -> 'a) -> 'a
