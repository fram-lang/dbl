(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unique identifier *)

(* Author: Piotr Polesiuk, 2023 *)

type t = int

let compare = Int.compare

let next_fresh = ref 0
let fresh () =
  let x = !next_fresh in
  next_fresh := x + 1;
  x

module Map = Map.Make(Int)
