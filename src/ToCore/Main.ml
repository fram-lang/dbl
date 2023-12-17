(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a translation from Unif to Core *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Translate expression *)
let rec tr_expr env (e : S.expr) =
  match e.data with
  | EUnit | EVar _ | EPureFn _ | EFn _ | ETFun _ ->
    tr_expr_v env e (fun v -> T.EValue v)

  | EApp(e1, e2) ->
    tr_expr_v env e1 (fun v1 ->
    tr_expr_v env e2 (fun v2 ->
    T.EApp(v1, v2)))

  | ETApp(e, tp) ->
    tr_expr_v env e (fun v ->
    let (Ex tp) = Type.tr_type env tp in
    T.ETApp(v, tp))

  | ELet(x, _, e1, e2) ->
    tr_expr_as env e1 x (tr_expr env e2)

  | EHandle(a, x, e, h, tp, eff) ->
    let tp  = Type.tr_ttype env tp in
    let eff = Type.tr_effect env eff in
    let h   = tr_h_expr env h in
    let (env, Ex a) = Env.add_tvar env a in
    begin match T.TVar.kind a with
    | KEffect -> T.EHandle(a, x, tr_expr env e, h, tp, eff)
    | KType -> failwith "Internal kind error"
    end

  | ERepl(func, eff) ->
    let eff = Type.tr_effect env eff in
    ERepl((fun () -> tr_expr env (func ())), eff)

  | EReplExpr(e1, tp, e2) ->
    EReplExpr(tr_expr env e1, tp, tr_expr env e2)

(** Translate expression and store result in variable [x] bound in [rest] *)
and tr_expr_as env (e : S.expr) x rest =
  match e.data with
  | EUnit | EVar _ | EPureFn _ | EFn _ | ETFun _ ->
    T.ELetPure(x, tr_expr env e, rest)

  | EApp _ | ETApp _ | ELet _ | EHandle _ | ERepl _ | EReplExpr _ ->
    T.ELet(x, tr_expr env e, rest)

(** Translate expression and pass a result (as a value to given
  meta-continuation) *)
and tr_expr_v env (e : S.expr) cont =
  match e.data with
  | EUnit  -> cont T.VUnit
  | EVar x -> cont (VVar x)

  | EPureFn(x, tp, body) | EFn(x, tp, body) ->
    let tp = Type.tr_ttype env tp in
    cont (VFn(x, tp, tr_expr env body))

  | ETFun(x, body) ->
    let (env, Ex x) = Env.add_tvar env x in
    cont (VTFun(x, tr_expr env body))

  | EApp _ | ETApp _ | EHandle _ | ERepl _ ->
    let x = Var.fresh () in
    T.ELet(x, tr_expr env e, cont (VVar x))

  | ELet(x, _, e1, e2) ->
    tr_expr_as env e1 x (tr_expr_v env e2 cont)

  | EReplExpr(e1, tp, e2) ->
    EReplExpr(tr_expr env e1, tp, tr_expr_v env e2 cont)

(** Translate a handler expression *)
and tr_h_expr env h =
  match h.data with
  | HEffect(tp_in, tp_out, x, r, body) ->
    let tp_in  = Type.tr_ttype env tp_in  in
    let tp_out = Type.tr_ttype env tp_out in
    T.HEffect(tp_in, tp_out, x, r, tr_expr env body)

(* ========================================================================= *)

let tr_program p =
  tr_expr Env.empty p