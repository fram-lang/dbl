(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a type inference *)
(*
open Common
open TypeCheckFix

(** Module of mutually recursive functions of the type-checker.
  See [TypeCheckFix] for more details. *)
module rec TCFix : TCFix = struct
  let infer_expr_type env e eff =
    Expr.infer_expr_type ~tcfix:(module TCFix) env e eff

  let check_expr_type env e tp eff =
    Expr.check_expr_type ~tcfix:(module TCFix) env e tp eff

  let check_def env ienv def tp_req eff cont =
    Def.check_def ~tcfix:(module TCFix) env ienv def tp_req eff cont

  let check_defs env ienv defs tp_req eff cont =
    Def.check_defs ~tcfix:(module TCFix) env ienv defs tp_req eff cont
end
*)
let tr_program p =
(*
  let (p, _) =
    TCFix.check_expr_type Env.empty p T.Type.t_unit T.Effect.io in
  InterpLib.Error.assert_no_error ();
  p *)
  failwith "not implemented: TypeInference.Main.tr_program"
