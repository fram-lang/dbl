(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a type inference *)

open Common

let tr_program p =
  let (p, _) =
    Expr.check_expr_type Env.empty p T.Type.t_unit T.Effect.io in
  InterpLib.Error.assert_no_error ();
  p
