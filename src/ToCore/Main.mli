(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a translation from Unif to Core *)

(** Translate program *)
val tr_program : repl_mode:bool -> Lang.Unif.program -> Lang.Core.program
