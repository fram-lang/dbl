(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

type t

(** Empty substitution *)
val empty : t

(** Extend substitution *)
val add_type : t -> tvar -> typ -> t

(** Substitute in type *)
val in_type : t -> typ -> typ
