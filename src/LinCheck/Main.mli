(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The main module of the linearity checking. *)

(** The main function of the translation. It decorates the program with
  linearity annotations, that will be used by the effect inference. *)
val tr_program : Lang.Unif.program -> Lang.Unif.program
