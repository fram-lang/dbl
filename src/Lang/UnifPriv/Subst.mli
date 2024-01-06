(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type t

(** Empty substitution *)
val empty : t

(** Extend substitution *)
val add_type : t -> tvar -> typ -> t

(** Substitute in type *)
val in_type : t -> typ -> typ

(** Substitute in constructor definition *)
val in_ctor_decl : t -> ctor_decl -> ctor_decl
