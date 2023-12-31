(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Generic variable representation. *)

(* Author: Piotr Polesiuk, 2023 *)

module Ordered = struct
  type t = {
    uid  : UID.t;
    name : string
  }

  let compare x y = UID.compare x.uid y.uid
end
include Ordered

let fresh ?(name="x") () =
  { uid  = UID.fresh ()
  ; name = name
  }

let unique_name x =
  x.name ^ UID.to_string x.uid

module Map = Map.Make(Ordered)
