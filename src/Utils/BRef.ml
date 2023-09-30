(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Backtrackable references *)

(* Author: Piotr Polesiuk, 2023 *)

type 'a t = 'a ref

let ref x = ref x

let get r = !r

let set r v = r := v
