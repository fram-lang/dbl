(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions related to the REPL. *)

open Common
open TypeCheckFix

let to_string_expr_default ~pos env tp =
  let make data =
    { T.pos  = pos;
      T.pp   = Env.pp_tree env;
      T.data = data
    } in
  let extern_tp =
    T.Type.t_arrow
      (T.Scheme.of_type tp)
      (T.Type.t_var T.BuiltinType.tv_string)
      Impure in
  { er_expr   = make (T.EExtern("dbl_toString", extern_tp));
    er_type   = Infered extern_tp;
    er_effect = Pure;
    er_constr = []
  }

let to_string_expr_method ~tcfix ~pos env self_tp meth_var sch =
  let pp = Env.pp_tree env in
  let make data =
    { T.pos  = pos;
      T.pp   = pp;
      T.data = data
    } in
  (* Create formal parameter of a function *)
  let self_var = Var.fresh ~name:"self" () in
  let self_expr =
    { er_expr   = make (T.EInst(make (T.EVar self_var), [], []));
      er_type   = Infered self_tp;
      er_effect = Pure;
      er_constr = []
    } in
  let meth_expr = make (T.EVar meth_var) in
  (* Build a method call *)
  let ctx = PolyExpr.method_call_ctx ~pos env self_expr in
  let body =
    PolyExpr.plug_inst_context ctx
      (Inst.instantiate_poly_expr ~tcfix ~pos env meth_expr sch []) in
  (* Ensure that the result type is string *)
  let body_tp = expr_result_type body in
  let str_tp = T.Type.t_var T.BuiltinType.tv_string in
  Error.check_unify_result ~pos
    (Unification.subtype env body_tp str_tp)
      ~on_error:(Error.expr_type_mismatch ~pp body_tp str_tp);
  (* Build the final function expression *)
  { er_expr   =
      make (T.EFn(
        self_var,
        T.SchemeExpr.of_type_expr (make (T.TE_Type self_tp)),
        body.er_expr,
        body.er_effect));
    er_type   = Infered
      (T.Type.t_arrow (T.Scheme.of_type self_tp) body_tp body.er_effect);
    er_effect = Pure;
    er_constr = body.er_constr
  }

let to_string_expr ~tcfix ~pos env tp =
  if not !DblConfig.repl_toString_printing then
    to_string_expr_default ~pos env tp
  else
    match NameUtils.method_owner_of_self tp with
    | None       -> to_string_expr_default ~pos env tp
    | Some owner ->
      begin match ModulePath.try_lookup_method ~pos env owner "toString" with
      | None   -> to_string_expr_default ~pos env tp
      | Some(meth_var, sch) ->
        to_string_expr_method ~tcfix ~pos env tp meth_var sch
      end
