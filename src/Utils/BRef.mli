(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Backtrackable references *)

(* Author: Piotr Polesiuk, 2021 *)

type 'a t

val create : 'a -> 'a t 

val equal : 'a t -> 'a t -> bool

val get : 'a t -> 'a

val set : 'a t -> 'a -> unit

(** Run computation and commit changes on success and
  * backtrack on failure *)
val bracket : (unit -> 'a) -> 'a
