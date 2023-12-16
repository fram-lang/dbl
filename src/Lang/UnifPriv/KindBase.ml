(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kind = kind_view
and kind_view =
  | KType
  | KEffect
  | KClEffect

let k_type = KType

let k_effect = KEffect

let k_cleffect = KClEffect

let rec view k =
  match k with
  | KType | KEffect | KClEffect -> k
