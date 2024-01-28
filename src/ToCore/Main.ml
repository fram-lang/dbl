(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a translation from Unif to Core *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Translate expression *)
let rec tr_expr env (e : S.expr) =
  match e.data with
  | EUnit | EVar _ | EPureFn _ | EFn _ | ETFun _ | ECtor _ | EHandler _ ->
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

  | EData(dds, e) ->
    let (env, dds) = DataType.tr_data_defs env dds in
    T.EData(dds, tr_expr env e)

  | EMatchEmpty(proof, me, tp, eff) ->
    let proof = tr_expr env proof in
    let tp  = Type.tr_ttype env tp in
    let eff = Type.tr_effect env eff in
    tr_expr_v env me (fun v ->
    T.EMatch(proof, v, [], tp, eff))

  | EMatch(me, cls, tp, eff) ->
    let tp  = Type.tr_ttype env tp in
    let eff = Type.tr_effect env eff in
    tr_expr_v env me (fun v ->
    PatternMatch.tr_single_match ~pos:e.pos ~env ~tr_expr v cls tp eff)

  | EHandle(a, x, e, he, tp, eff) ->
    tr_expr_v env he (fun hv ->
    let l   = Var.fresh () in
    let tp  = Type.tr_ttype env tp in
    let eff = Type.tr_effect env eff in
    let (env, Ex a) = Env.add_tvar env a in
    let hx  = Var.fresh () in
    let rx     = Var.fresh () in
    let r_body = T.EValue (T.VVar rx) in
    begin match T.TVar.kind a with
    | KEffect ->
      T.ELabel(a, l, tp, eff,
        T.ELet(hx, T.ETApp(hv, T.TVar a),
          T.ELet(x, T.EApp(T.VVar hx, T.VVar l),
            T.EReset(T.VVar l, tr_expr env e, rx, r_body))))
    | KType | KArrow _ -> failwith "Internal kind error"
    end)

  | ERepl(func, tp, eff) ->
    let tp  = Type.tr_ttype  env tp  in
    let eff = Type.tr_effect env eff in
    ERepl((fun () -> tr_expr env (func ())), tp, eff)

  | EReplExpr(e1, tp, e2) ->
    EReplExpr(tr_expr env e1, tp, tr_expr env e2)

(** Translate expression and store result in variable [x] bound in [rest] *)
and tr_expr_as env (e : S.expr) x rest =
  match e.data with
  | EUnit | EVar _ | EPureFn _ | EFn _ | ETFun _ | ECtor _ | EHandler _ ->
    T.ELetPure(x, tr_expr env e, rest)

  | EApp _ | ETApp _ | ELet _ | EData _ | EMatchEmpty _ | EMatch _ | EHandle _
  | ERepl _ | EReplExpr _ ->
    T.ELet(x, tr_expr env e, rest)

(** Translate expression and pass a result (as a value to given
  meta-continuation) *)
and tr_expr_v env (e : S.expr) cont =
  match e.data with
  | EUnit  -> cont T.VUnit
  | EVar x -> cont (VVar x)

  | EPureFn(x, sch, body) | EFn(x, sch, body) ->
    let tp = Type.tr_scheme env sch in
    cont (VFn(x, tp, tr_expr env body))

  | ETFun(x, body) ->
    let (env, Ex x) = Env.add_tvar env x in
    cont (VTFun(x, tr_expr env body))

  | EApp _ | ETApp _ | EMatchEmpty _ | EMatch _ | EHandle _ | ERepl _ ->
    let x = Var.fresh () in
    T.ELet(x, tr_expr env e, cont (VVar x))

  | ELet(x, _, e1, e2) ->
    tr_expr_as env e1 x (tr_expr_v env e2 cont)

  | ECtor(proof, n, tps, args) ->
    let proof = tr_expr env proof in
    let tps   = List.map (Type.tr_type env) tps in
    tr_expr_vs env args (fun args ->
    cont (VCtor(proof, n, tps, args)))

  | EData(dds, e) ->
    let (env, dds) = DataType.tr_data_defs env dds in
    T.EData(dds, tr_expr_v env e cont)

  | EHandler(a, tp, eff, h) ->
    let (env, Ex a) = Env.add_tvar env a in
    begin match T.TVar.kind a with
    | KEffect ->
      let tp  = Type.tr_ttype  env tp in
      let eff = Type.tr_effect env eff in
      let l   = Var.fresh ~name:"lbl" () in
      cont (T.VTFun(a, T.EValue(T.VFn(l, TLabel(TVar a, tp, eff),
        tr_h_expr env (T.VVar l) h))))
    | _ ->
      failwith "Internal kind error"
    end

  | EReplExpr(e1, tp, e2) ->
    EReplExpr(tr_expr env e1, tp, tr_expr_v env e2 cont)

(** Translate a list of expressions and pass a result (as a list of values to
  given meta-continuation) *)
and tr_expr_vs env es cont =
  match es with
  | []      -> cont []
  | e :: es ->
    tr_expr_v env  e  (fun v ->
    tr_expr_vs env es (fun vs ->
    cont (v :: vs)))

(** Translate a handler expression *)
and tr_h_expr env lbl (h : S.h_expr) =
  match h.data with
  | HEffect(tp_in, tp_out, x, r, body) ->
    let tp_in  = Type.tr_scheme env tp_in in
    let tp_out = Type.tr_ttype env tp_out in
    T.EValue (T.VFn(x, tp_in, T.EShift(lbl, r, tr_expr env body, tp_out)))

(* ========================================================================= *)

let tr_program p =
  tr_expr Env.empty p
