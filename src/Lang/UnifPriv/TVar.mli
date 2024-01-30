(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open KindBase

type t = private {
  uid  : UID.t;
  kind : kind
}

val kind : t -> kind

val fresh : kind -> t

val clone : t -> t

val equal : t -> t -> bool

val uid : t -> UID.t

module Set  : Set.S with  type elt = t
module Map  : Map.S with  type key = t
module Perm : Perm.S with type key = t and module KeySet = Set
