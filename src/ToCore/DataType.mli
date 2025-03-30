(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of datatype definitions from ConE to Core *)
open Common

(** Translate a list of mutually recursive datatype definitions *)
val tr_data_defs : Env.t -> S.data_def list -> Env.t * T.data_def list
