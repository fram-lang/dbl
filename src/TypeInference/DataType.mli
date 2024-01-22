(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Open ADT making its constructors immediately available. *)
val open_data : Env.t -> Env.adt_info -> Env.t

(** Check non-recursive definition of ADT *)
val check_data_def : Env.t -> S.data_def -> Env.t * T.data_def

(** Check mutually recursive definitions of ADTs *)
val check_rec_data_defs : Env.t -> S.data_def list -> Env.t * T.data_def list
