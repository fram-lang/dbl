(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

open KindBase

module Ordered = struct
  type t = {
    uid  : UID.t;
    kind : kind
  }

  let compare x y = UID.compare x.uid y.uid
end
include Ordered

let kind x = x.kind

let fresh kind =
  { uid  = UID.fresh ()
  ; kind = kind
  }

let clone x = fresh (kind x)

let equal x y = x == y

let uid x = x.uid

module Set  = Set.Make(Ordered)
module Map  = Map.Make(Ordered)
module Perm = Perm.Make(Ordered)(Set)
