(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a translation from Unif to Core *)

open Common

(** Translate expression *)
let rec tr_expr env (e : S.expr) =
  match e.data with
  | EUnitPrf | ENum _ | EStr _ | EVar _ | EPureFn _ | EFn _ | ETFun _
  | ECtor _ | EHandler _ | EExtern _ ->
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

  | ELabel(a, l, tp, eff, e) ->
    let (env, Ex a) = Env.add_tvar env a in
    let tp  = Type.tr_ttype  env tp  in
    let eff = Type.tr_effect env eff in
    begin match T.TVar.kind a with
    | KEffect ->
      T.ELabel(a, l, tp, eff, tr_expr env e)
    | KType | KArrow _ -> failwith "Internal kind error"
    end

  | EHandle { label; effect; cap_var; body; capability;
      ret_var; ret_body; result_tp; result_eff } ->
    tr_expr_v env label (fun lv ->
    tr_expr_v env capability (fun hv ->
    let h_eff = Type.tr_effect env effect in
    let hx  = Var.fresh () in
    let r_body = tr_expr env ret_body in
    T.ELet(hx, T.ETApp(hv, h_eff),
      T.ELet(cap_var, T.EApp(T.VVar hx, lv),
        T.EReset(lv, tr_expr env body, ret_var, r_body)))))

  | EEffect(l, x, body, tp) ->
    tr_expr_v env l (fun lv ->
    T.EShift(lv, x, tr_expr env body, Type.tr_ttype env tp))

  | ERepl(func, tp, eff) ->
    let tp  = Type.tr_ttype  env tp  in
    let eff = Type.tr_effect env eff in
    ERepl((fun () -> tr_expr env (func ())), tp, eff)

  | EReplExpr(e1, tp, e2) ->
    EReplExpr(tr_expr env e1, tp, tr_expr env e2)

(** Translate expression and store result in variable [x] bound in [rest] *)
and tr_expr_as env (e : S.expr) x rest =
  match e.data with
  | EUnitPrf | ENum _ | EStr _ | EVar _ | EPureFn _ | EFn _ | ETFun _
  | ECtor _ | EHandler _ | EExtern _ ->
    T.ELetPure(x, tr_expr env e, rest)

  | EApp _ | ETApp _ | ELabel _ | ELet _ | EData _ | EMatchEmpty _ | EMatch _
  | EHandle _ | EEffect _ | ERepl _ | EReplExpr _ ->
    T.ELet(x, tr_expr env e, rest)

(** Translate expression and pass a result (as a value to given
  meta-continuation) *)
and tr_expr_v env (e : S.expr) cont =
  match e.data with
  | EUnitPrf -> cont v_unit_prf
  | ENum n -> cont (VNum n)
  | EStr s -> cont (VStr s)
  | EVar x -> cont (VVar x)

  | EPureFn(x, sch, body) | EFn(x, sch, body) ->
    let tp = Type.tr_scheme env sch in
    cont (VFn(x, tp, tr_expr env body))

  | ETFun(x, body) ->
    let (env, Ex x) = Env.add_tvar env x in
    cont (VTFun(x, tr_expr env body))

  | EApp _ | ETApp _ | EMatchEmpty _ | EMatch _ | ELabel _ | EHandle _
  | EEffect _ | ERepl _ ->
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

  | EHandler(a, lx, tp, eff, h) ->
    let (env, Ex a) = Env.add_tvar env a in
    begin match T.TVar.kind a with
    | KEffect ->
      let tp  = Type.tr_ttype  env tp in
      let eff = Type.tr_effect env eff in
      cont (T.VTFun(a, T.EValue(T.VFn(lx, TLabel(TVar a, tp, eff),
        tr_expr env h))))
    | _ ->
      failwith "Internal kind error"
    end

  | EExtern(name, tp) -> cont (VExtern(name, Type.tr_ttype env tp))

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

(* ========================================================================= *)

let tr_program p =
  tr_expr Env.empty p
