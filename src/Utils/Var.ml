(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Generic variable representation. *)

(* Author: Piotr Polesiuk, 2023 *)

type t = {
  uid  : UID.t;
  name : string
}

let fresh ?(name="x") () =
  { uid  = UID.fresh ()
  ; name = name
  }
