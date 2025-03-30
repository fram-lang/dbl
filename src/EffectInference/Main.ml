(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The main module of the effect inference *)

open Common

let tr_program ~solve_all p =
  let env = Env.initial ~solve_all () in
  let (e, Checked) =
    Expr.check_type env p
      (T.Type.t_var T.BuiltinType.tv_unit)
      (Check T.CEffect.prog_effect) in
  ConstrSolver.final_solve env;
  e
