(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type t

val empty : t

val singleton : 'k tvar -> 'k typ -> t

val add : t -> 'k tvar -> 'k typ -> t

val in_type : t -> 'k typ -> 'k typ
