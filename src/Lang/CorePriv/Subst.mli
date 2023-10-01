(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

type t

val singleton : 'k tvar -> 'k typ -> t

val in_type : t -> 'k typ -> 'k typ
