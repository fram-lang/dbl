(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Built-in type variable *)

(** Int type *)
let tv_int = TVar.fresh ~scope:Scope.initial Kind.k_type

(** Int64 type *)
let tv_int64 = TVar.fresh ~scope:Scope.initial Kind.k_type

(** String type *)
let tv_string = TVar.fresh ~scope:Scope.initial Kind.k_type

(** Char type *)
let tv_char = TVar.fresh ~scope:Scope.initial Kind.k_type

(** Unit type *)
let tv_unit = TVar.fresh ~scope:Scope.initial Kind.k_type

(** Bool type *)
let tv_bool = TVar.fresh ~scope:Scope.initial Kind.k_type

(** Option type *)
let tv_option = TVar.fresh ~scope:Scope.initial
  (Kind.k_noneff_arrow Kind.k_type Kind.k_type)

(** IO effect *)
let tv_io = TVar.fresh ~scope:Scope.initial Kind.k_effect

(** List of all built-in types together with their names *)
let all =
  [ "Int",    tv_int;
    "Int64",  tv_int64;
    "String", tv_string;
    "Char",   tv_char;
    "Unit",   tv_unit;
    "Bool",   tv_bool;
    "Option", tv_option;
    "IO",     tv_io ]
