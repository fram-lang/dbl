(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Evaluator *)

exception Runtime_error

val eval_program : Lang.Untyped.program -> unit
