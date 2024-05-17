(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Built-in type variable *)

(** Int type *)
let tv_int = TVar.fresh KindBase.k_type

(** String type *)
let tv_string = TVar.fresh KindBase.k_type

(** Char type *)
let tv_char = TVar.fresh KindBase.k_type

(** Unit type *)
let tv_unit = TVar.fresh KindBase.k_type

(** IO effect *)
let tv_io = TVar.fresh KindBase.k_effect

(** List of all built-in types together with their names *)
let all =
  [ "Int",    tv_int;
    "String", tv_string;
    "Char",   tv_char;
    "Unit",   tv_unit;
    "IO",     tv_io ]
