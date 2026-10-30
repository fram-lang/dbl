(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The main module of the linearity checking. *)

let tr_program p =
  let (p, _, _) = Expr.tr_expr Env.initial p in
  p
