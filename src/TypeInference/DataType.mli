(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Check declarations of constructors *)
val check_ctor_decls : Env.t -> S.ctor_decl list -> T.ctor_decl list

(** Open ADT making its constructors immediately available. *)
val open_data : Env.t -> Env.adt_info -> Env.t
