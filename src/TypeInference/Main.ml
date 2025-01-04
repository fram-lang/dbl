(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a type inference *)

open Common
open TypeCheckFix

(** Module of mutually recursive functions of the type-checker.
  See [TypeCheckFix] for more details. *)
module rec TCFix : TCFix = struct
  let infer_expr_type ?app_type env e =
    Expr.infer_expr_type ~tcfix:(module TCFix) ?app_type env e

  let check_expr_type env e tp =
    Expr.check_expr_type ~tcfix:(module TCFix) env e tp

  let check_def env ienv def tp_req cont =
    Def.check_def ~tcfix:(module TCFix) env ienv def tp_req cont

  let check_defs env ienv defs tp_req cont =
    Def.check_defs ~tcfix:(module TCFix) env ienv defs tp_req cont
end

let tr_program p =
  let er = TCFix.check_expr_type Env.empty p T.Type.t_unit in
  Constr.solve_all er.er_constr;
  InterpLib.Error.assert_no_error ();
  er.er_expr
