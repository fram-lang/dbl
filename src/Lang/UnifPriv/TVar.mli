(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

open KindBase

type t = private {
  uid    : UID.t;
  pp_uid : PPTree.uid;
  kind   : kind;
  scope  : Scope.t
}

val kind : t -> kind

val fresh : ?pp_uid:PPTree.uid -> scope:Scope.t -> kind -> t

val clone : scope:Scope.t -> t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val uid : t -> UID.t

val pp_uid : t -> PPTree.uid

val scope : t -> Scope.t

module Set : Set.S with  type elt = t
module Map : Map.S with  type key = t
