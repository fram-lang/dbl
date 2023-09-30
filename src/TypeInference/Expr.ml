(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(* ------------------------------------------------------------------------- *)
(** Infer scheme of type variable *)
let infer_var_scheme ~pos env x =
  match Env.lookup_var env x with
  | Some (x, sch) ->
    ({ T.pos = pos; T.data = T.EVar x }, sch)
  | None ->
    Error.fatal (Error.unbound_var ~pos x)

(* ------------------------------------------------------------------------- *)
(** Infer type of an expression *)
let rec infer_expr_type env (e : S.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit ->
    (make T.EUnit, T.Type.t_unit)

  | EVar x ->
    let (e, sch) = infer_var_scheme ~pos:e.pos env x in
    ExprUtils.instantiate env e sch

  | EFn(x, body) ->
    let tp1 = T.Type.fresh_uvar T.Kind.k_type in
    let (env, x) = Env.add_mono_var env x tp1 in
    let (body, tp2) = infer_expr_type env body in
    (make (T.EFn(x, tp1, body)), T.Type.t_arrow tp1 tp2)

  | EApp(e1, e2) ->
    let (e1, ftp) = infer_expr_type env e1 in
    begin match Subtyping.to_arrow env ftp with
    | Some(atp, vtp) ->
      let e2 = check_expr_type env e2 atp in
      (make (T.EApp(e1, e2)), vtp)
    | None ->
      Error.fatal (Error.expr_not_function ~pos:e1.pos ~env ftp)
    end

  | ELetV(x, e1, e2) ->
    let (env, x, sch, e1) = check_let_v env x e1 in
    let (e2, tp) = infer_expr_type env e2 in
    (make (T.ELet(x, sch, e1, e2)), tp)

  | ELetE(x, e1, e2) ->
    let (env, x, sch, e1) = check_let_e env x e1 in
    let (e2, tp) = infer_expr_type env e2 in
    (make (T.ELet(x, sch, e1, e2)), tp)

(* ------------------------------------------------------------------------- *)
(** Check type of an expression *)
and check_expr_type env (e : S.expr) tp =
  let make data = { e with data = data } in
  match e.data with
  | EUnit | EVar _ | EApp _ ->
    let pos = e.pos in
    let (e, tp') = infer_expr_type env e in
    if not (Subtyping.subtype env tp' tp) then
      Error.report (Error.expr_type_mismatch ~pos ~env tp' tp);
    e

  | EFn(x, body) ->
    begin match Subtyping.from_arrow env tp with
    | Some(tp1, tp2) ->
      let (env, x) = Env.add_mono_var env x tp1 in
      let body = check_expr_type env body tp2 in
      make (T.EFn(x, tp1, body))
    | None ->
      Error.report (Error.expr_not_function_ctx ~pos:e.pos ~env tp);
      fst (infer_expr_type env e)
    end

  | ELetV(x, e1, e2) ->
    let (env, x, sch, e1) = check_let_v env x e1 in
    let e2 = check_expr_type env e2 tp in
    make (T.ELet(x, sch, e1, e2))

  | ELetE(x, e1, e2) ->
    let (env, x, sch, e1) = check_let_e env x e1 in
    let e2 = check_expr_type env e2 tp in
    make (T.ELet(x, sch, e1, e2))

(* ------------------------------------------------------------------------- *)
(** Check polymorphic let-definition *)
and check_let_v env x body =
  let (body, tp) = infer_expr_type env body in
  let (body, sch) = ExprUtils.generalize env body tp in
  let (env, x) = Env.add_poly_var env x sch in
  (env, x, sch, body)

(* ------------------------------------------------------------------------- *)
(** Check monomorphic let-definition *)
and check_let_e env x body =
  let (body, tp) = infer_expr_type env body in
  let sch = { T.sch_tvars = []; T.sch_body = tp } in
  let (env, x) = Env.add_poly_var env x sch in
  (env, x, sch, body)
