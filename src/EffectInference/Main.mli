(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The main module of the effect inference *)

(** The main function of the translation. The [solve_all] flag indicates the
  behavior of the constraint solving: when is set, all constraints are solved,
  otherwise, it only checks if constraints are solvable. *)
val tr_program : solve_all:bool -> Lang.Unif.program -> Lang.ConE.program
