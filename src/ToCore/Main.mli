(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a translation from ConE to Core *)

(** Translate program *)
val tr_program : Lang.ConE.program -> Lang.Core.program
