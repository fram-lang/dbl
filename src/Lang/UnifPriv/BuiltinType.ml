(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Built-in type variable *)

(* 2024: Piotr Polesiuk: initial implementation *)

(** Int type *)
let tv_int = TVar.fresh KindBase.k_type

(** List of all built-in types together with their names *)
let all =
  [ "Int", tv_int ]
