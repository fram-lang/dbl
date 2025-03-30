(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of a translation from ConE to Core *)

open Common

(** Expression-building do-notation *)
let (let^ ) m cont = m cont
let (let* ) m f cont = m (Fun.flip f cont)
let return x cont = cont x

(** Translate expression *)
let rec tr_expr env (e : S.expr) =
  match e with
  | EUnitPrf | EOptionPrf | ENum _ | ENum64 _ | EStr _ | EChr _ | EVar _
  | EFn _ | ETFun _ | ECAbs _ | ECtor _ | EExtern _ | ERepl _ | EReplExpr _ ->
    let^ v = tr_expr_v env e in
    T.EValue v

  | EApp(e1, e2) ->
    let^ v1 = tr_expr_v env e1 in
    let^ v2 = tr_expr_v env e2 in
    T.EApp(v1, v2)

  | ETApp(e, tp) ->
    let^ v = tr_expr_v env e in
    let (Ex tp) = Type.tr_type env tp in
    T.ETApp(v, tp)

  | ECApp e ->
    let^ v = tr_expr_v env e in
    T.ECApp v

  | ELet(x, e1, e2) ->
    let^ () = tr_let_expr ~pure:false x env e1 in
    tr_expr env e2

  | ELetPure(x, e1, e2) ->
    let^ () = tr_let_expr ~pure:true x env e1 in
    tr_expr env e2

  | ELetRec(defs, body) ->
    let defs = tr_rec_defs env defs in
    T.ELetRec(defs, tr_expr env body)

  | ERecCtx e ->
    T.ERecCtx(tr_expr env e)

  | EData(dds, e) ->
    let (env, dds) = DataType.tr_data_defs env dds in
    T.EData(dds, tr_expr env e)

  | EMatch(prf, e, cls, tp, eff) ->
    let^ v = tr_expr_v env e in
    T.EMatch(tr_expr env prf, v,
      List.map (tr_match_clause env) cls,
      Type.tr_ttype env tp,
      Type.tr_ceffect env eff)

  | EShift(lbl_e, x, body, tp) ->
    let^ lbl_v = tr_expr_v env lbl_e in
    T.EShift(lbl_v, [], [], x, tr_expr env body, Type.tr_ttype env tp)

  | EReset(lbl_e, body, ret_var, ret_body) ->
    let^ lbl_v = tr_expr_v env lbl_e in
    T.EReset(lbl_v, [], [], tr_expr env body, ret_var, tr_expr env ret_body)

(** Translate expression and store result in variable [x] *)
and tr_let_expr ~pure x env (e : S.expr) cont =
  match e with
  | _ when pure ->
    T.ELetPure(x, tr_expr env e, cont ())

  | EUnitPrf | EOptionPrf | ENum _ | ENum64 _ | EStr _ | EChr _ | EVar _
  | EFn _ | ETFun _ | ECAbs _ | EExtern _ ->
    T.ELetPure(x, tr_expr env e, cont ())

  | EApp _ | ETApp _ | ECApp _ | ELet _ | ELetPure _ | ELetRec _ | ERecCtx _
  | EData _ | ECtor _ | EMatch _ | EShift _ | EReset _
  | ERepl _ | EReplExpr _ ->
    T.ELet(x, tr_expr env e, cont ())

and tr_expr_as_var env e =
  let x = Var.fresh () in
  let* () = tr_let_expr ~pure:false x env e in
  return x

and tr_expr_v env (e : S.expr) =
  match e with
  | EUnitPrf   -> return v_unit_prf
  | EOptionPrf -> return v_option_prf

  | ENum   n -> return (T.VNum n)
  | ENum64 n -> return (T.VNum64 n)
  | EStr   s -> return (T.VStr s)
  | EChr   c -> return (T.VNum (Char.code c))
  | EVar   x -> return (T.VVar x)

  | EFn(x, sch, body) ->
    let tp = Type.tr_scheme env sch in
    return (T.VFn(x, tp, tr_expr env body))

  | ETFun(x, body) ->
    let (env, Ex x) = Env.add_tvar env x in
    return (T.VTFun(x, tr_expr env body))

  | ECAbs(cs, body) ->
    return (T.VCAbs(List.map (Type.tr_constr env) cs, tr_expr env body))

  | ELet(x, e1, e2) ->
    let* () = tr_let_expr ~pure:false x env e1 in
    tr_expr_v env e2

  | ELetPure(x, e1, e2) ->
    let* () = tr_let_expr ~pure:true x env e1 in
    tr_expr_v env e2

  | ECtor(prf, idx, tps, args)  ->
    let prf = tr_expr env prf in
    let tps = List.map (Type.tr_type env) tps in
    let* args = tr_expr_vs env args in
    return (T.VCtor(prf, idx, tps, args))

  | EExtern(name, tp) ->
    return (T.VExtern(name, Type.tr_ttype env tp))

  | EApp _ | ETApp _ | ECApp _ | ELetRec _ | ERecCtx _ | EData _ | EMatch _
  | EShift _ | EReset _ | ERepl _ | EReplExpr _ ->
    let* x = tr_expr_as_var env e in
    return (T.VVar x)

(** Translate a list of expressions as list of values in expression building
  monad. *)
and tr_expr_vs env es =
  match es with
  | [] -> return []
  | e :: es ->
    let* v = tr_expr_v env e in
    let* vs = tr_expr_vs env es in
    return (v :: vs)

(** Translate a match clause *)
and tr_match_clause env (cl : S.match_clause) =
  let (env, tvs) = Env.add_tvars env cl.cl_tvars in
  { T.cl_tvars = tvs;
    T.cl_vars  = cl.cl_vars;
    T.cl_body  = tr_expr env cl.cl_body
  }

(** Translate recursive definitions *)
and tr_rec_defs env defs =
  List.map (tr_rec_def env) defs

and tr_rec_def env (rd : S.rec_def) =
  let (env, tvs) = Env.add_tvars env rd.rd_evars in
  let cs   = List.map (Type.tr_constr env) rd.rd_constr in
  let sch  = Type.tr_scheme env rd.rd_scheme in
  let tp   = T.Type.t_foralls tvs (TGuard(cs, sch)) in
  let body = tr_expr env rd.rd_body in
  (rd.rd_var, tp, body)

(* ========================================================================= *)

let tr_program p =
  tr_expr Env.initial p
