(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Names of types and values. *)

(** Name of a named type parameter *)
type tname =
  | TNAnon
  | TNVar of string

(** Name of a named parameter *)
type name =
  | NVar of string
  | NOptionalVar of string
  | NImplicit of string
  | NMethod of string

(** Linear order on tname values *)
val compare_tname : tname -> tname -> int

(** Linear order on name values *)
val compare : name -> name -> int

(** Equality test on names *)
val equal : name -> name -> bool
