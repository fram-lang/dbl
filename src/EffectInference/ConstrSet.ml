(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Mutable sets of constraints *)

type t = Constr.t list BRef.t

let create () = BRef.create []

let add cset c = BRef.set cset (c :: BRef.get cset)

let add_list cset cs = BRef.set cset (cs @ BRef.get cset)

let to_list cset = BRef.get cset

let clear cset = BRef.set cset []
