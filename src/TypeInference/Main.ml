(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a type inference *)

(* Author: Piotr Polesiuk, 2023 *)

let tr_program p =
  let p = Expr.check_expr_type Env.empty p Lang.Unif.Type.t_unit in
  InterpLib.Error.assert_no_error ();
  p
