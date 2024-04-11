(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

open Common

(** Check non-recursive definition of ADT *)
val check_data_def : Env.t -> S.data_def -> Env.t * T.data_def

(** Check mutually recursive definitions of ADTs *)
val check_rec_data_defs : Env.t -> S.data_def list -> Env.t * T.data_def list
