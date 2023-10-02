(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kind = kind_view
and kind_view =
  | KType
  | KEffrow

let k_type = KType

let k_effrow = KEffrow

let rec view k =
  match k with
  | KType | KEffrow -> k
