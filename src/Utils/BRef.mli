(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Backtrackable references *)

(* Author: Piotr Polesiuk, 2023 *)

type 'a t

(** Create a new reference *)
val ref : 'a -> 'a t

(** Get the value stored in a mutable cell *)
val get : 'a t -> 'a

(** Set a new value of a mutable cell *)
val set : 'a t -> 'a -> unit
