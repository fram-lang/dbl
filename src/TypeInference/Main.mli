(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a type inference *)

(** Infer types in a program and translate it to the Unif language *)
val tr_program : Lang.Surface.program -> Lang.Unif.program
