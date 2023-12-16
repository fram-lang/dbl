(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

(* Author: Piotr Polesiuk, 2023 *)

open KindBase

type t = private {
  uid  : UID.t;
  kind : kind
}

val kind : t -> kind

val fresh : kind -> t

val equal : t -> t -> bool

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
