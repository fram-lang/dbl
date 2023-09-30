(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

(* Author: Piotr Polesiuk, 2023 *)

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

let equal x y = x == y

module Map = Map.Make(Ordered)
