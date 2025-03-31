(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of datatype definitions. *)

open Common

(** Translate datatype definitions and add them to the environment. Returns the
  updated environment, the translated datatype definitions, and the list of type
  variables that were introduced. *)
val tr_data_defs :
  Env.t -> S.data_def list -> Env.t * T.data_def list * T.tvar list
