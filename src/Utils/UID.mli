(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unique identifier *)

type t = private int

(** Comparator *)
val compare : t -> t -> int

(** Generate fresh identifier *)
val fresh : unit -> t

(** Return a string that uniquely identify the UID *)
val to_string : t -> string

(** Finite maps from UIDs *)
module Map : Map.S with type key = t
