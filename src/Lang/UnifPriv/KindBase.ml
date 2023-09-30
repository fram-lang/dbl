(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kind = kind_view
and kind_view =
  | KType

let k_type = KType
