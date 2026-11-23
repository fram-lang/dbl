(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Transforming expressions. *)

open Lang.Unif

val tr_expr : Env.t -> expr -> expr * Resource.typ * Resource.usage
