(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Built-in type variable *)

(* 2024: Piotr Polesiuk: initial implementation *)

open TypeBase

(** Int type *)
let tv_int = TVar.fresh KType

(** String type *)
let tv_string = TVar.fresh KType

(** IO effect *)
let tv_io = TVar.fresh KEffect

(** Possible non-termination effect *)
let tv_nterm = TVar.fresh KEffect

(** List of all built-in types together with their names *)
let all =
  [ "Int",    TVar.Ex tv_int;
    "String", TVar.Ex tv_string;
    "IO",     TVar.Ex tv_io;
    "#NTerm", TVar.Ex tv_nterm ]
