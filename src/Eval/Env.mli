(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value

type t

val empty : t

val extend : t -> Var.t -> value -> t

val lookup : t -> Var.t -> value

val begin_fix  : t -> t

val update_fix : t -> t -> unit
