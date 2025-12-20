(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of expressions *)

open Common

(** Build a response for effect-checking when returning a pure effect. *)
let return_pure (type ed) (req : (T.ceffect, ed) request) :
    (T.ceffect, ed) response =
  match req with
  | Infer   -> Infered Pure
  | Check _ -> Checked

(** Build a response for effect-checking *)
let return_effect (type ed) ~(node : _ S.node) env
    (req : (T.ceffect, ed) request) (eff : T.ceffect) :
    (T.ceffect, ed) response =
  match req, eff with
  | Infer, _ -> Infered eff

  | Check _, Pure -> Checked

  | Check (Impure eff'), Impure eff ->
    let origin = OExprEffect(node.pos, node.pp, eff, eff') in
    ConstrSolver.add_constraint ~origin env eff eff';
    Checked

  | Check Pure, Impure _ ->
    failwith "Internal effect error"

(** Join effect responses from multiple branches *)
let eff_resp_join (type ed)
  (eff : (T.ceffect, ed) response) (effs : (T.ceffect, ed) response list) :
    (T.ceffect, ed) response =
  match eff with
  | Infered eff ->
    Infered (List.fold_left
      (fun eff (Infered eff') -> T.CEffect.join eff eff')
      eff effs)

  | Checked -> Checked

(** Check effect annotation against effect request. Returns expected effect
  of the expression and the effect response *)
let effect_annot (type ed) env
    (eff : S.effct) (eff_req : (T.ceffect, ed) request) :
    T.ceffect * (T.ceffect, ed) response =
  match eff, eff_req with
  | Pure, _ -> (Pure, return_pure eff_req)
  
  | Impure, Infer ->
    let eff = T.Impure (Env.fresh_gvar env) in
    (eff, Infered eff)

  | Impure, Check (Impure eff) ->
    (Impure eff, Checked)

  | Impure, Check Pure ->
    failwith "Internal effect error"

(* ========================================================================= *)

(** Translate a named parameter and add it to the environment. *)
let tr_named_param env (name, x, sch) =
  let sch = Type.tr_scheme_expr env sch in
  let env = Env.add_poly_var env x sch in
  (env, (name, x, sch))

(** Translate a list of named parameters and add them to the environment. *)
let tr_named_params env named =
  List.fold_left_map tr_named_param env named

(* ========================================================================= *)

let guess_effects env evs =
  List.fold_left_map
    (fun sub x ->
      let tp = T.Type.t_effect (Env.fresh_gvar env) in
      (T.Subst.add sub x tp, tp))
    T.Subst.empty
    evs

let instantiate_constraints ~origin sub cs =
  List.map
    (fun (eff1, eff2) ->
      Constr.CSubeffect(origin,
        T.Effct.subst sub eff1,
        T.Effct.subst sub eff2))
    cs

(** Infer a scheme of a polymorphic expression. This function is slightly more
  general than [infer_scheme]: it takes additional list of named parameters
  that should be abstracted after abstracting all type parameters. *)
let rec tr_poly_expr env named (e : S.poly_expr) =
  match e.data with
  | EVar x ->
    let (env, named) = tr_named_params env named in
    let (e, sch) =
      match Env.lookup_var env x with
      | Simple sch -> (T.EVar x, sch)
      | Large(evs, cs, sch) ->
        let (sub, tps) = guess_effects env evs in
        let origin = OInst(e.pos, e.pp, evs, cs, sch) in
        Env.add_constraints env (instantiate_constraints ~origin sub cs);
        let sch = T.Scheme.subst sub sch in
        (ExprUtils.mk_linst (T.EVar x) tps, sch)
    in
    ExprUtils.generalize [] named e sch

  | ECtor([], prf, idx) ->
    let (env, named) = tr_named_params env named in
    let (prf, tp, ctors, _) = ProofExpr.tr_proof_expr env prf in
    let (e, sch) = ExprUtils.mk_ctor ~prf ~idx tp ctors in
    ExprUtils.generalize [] named e sch

  | ECtor(targs, prf, idx) ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, targs) = Env.add_named_tvars env targs in
    let (env, named) = tr_named_params env named in
    let (prf, tp, ctors, _) = ProofExpr.tr_proof_expr env prf in
    let (e, sch) = ExprUtils.mk_ctor ~prf ~idx tp ctors in
    let (e, sch) = ExprUtils.generalize targs named e sch in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:(List.map snd targs)
      (Env.constraints env) sch;
    (e, sch)

  | EPolyFun([], named2, body) ->
    let (env, named) = tr_named_params env (named @ named2) in 
    let (body, tp, Checked) = infer_type env body (Check Pure) in
    ExprUtils.generalize [] named body (T.Scheme.of_type tp)

  | EPolyFun(targs, named2, body) ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, targs) = Env.add_named_tvars env targs in
    let (env, named) = tr_named_params env (named @ named2) in
    let (body, tp, Checked) = infer_type env body (Check Pure) in
    let (e, sch) =
      ExprUtils.generalize targs named body (T.Scheme.of_type tp) in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:(List.map snd targs)
      (Env.constraints env) sch;
    (e, sch)

  | EGen([], named2, body) ->
    tr_poly_expr env (named @ named2) body

  | EGen(targs, named2, body) ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, targs) = Env.add_named_tvars env targs in
    let (body, sch) = tr_poly_expr env (named @ named2) body in
    let body = ExprUtils.mk_named_tfuns targs body in
    let sch = { sch with sch_targs = targs @ sch.sch_targs } in
    ConstrSolver.leave_scope_with_scheme
      ~env0 ~tvars:(List.map snd targs) (Env.constraints env) sch;
    (body, sch)

(** Infer a scheme of a polymorphic expression. *)
and infer_scheme env e =
  tr_poly_expr env [] e

(* ========================================================================= *)

(** Translate and generalize a let-polymorphic expression. *)
and tr_let_poly env x body =
  let env0 = env in
  let env = Env.enter_scope env in
  let (e, sch) = infer_scheme env body in
  let (evs, cs) =
    ConstrSolver.generalize_with_scheme ~env0 (Env.constraints env) sch in
  let env = env0 in
  match evs, cs with
  | [], [] ->
    let env = Env.add_poly_var env x sch in
    (env, e)
  | _ ->
    let e = ExprUtils.generalize_constr evs cs e in
    let env = Env.add_lpoly_var env x evs cs sch in
    (env, e)

(* ========================================================================= *)

and tr_let_rec env targs named defs =
  let env0 = env in
  let env = Env.enter_scope env in
  let (targs, named, defs) = tr_let_rec_defs env targs named defs in
  let (evs, cs) =
    ConstrSolver.generalize_with_schemes ~env0 (Env.constraints env)
      (List.map (fun (_, _, sch, _) -> sch) defs) in
  let env = env0 in
  let rec_ctx =
    ExprUtils.mk_rec_ctx ~evs ~cs ~targs ~named
      (List.map
        (fun (rd, _, _, sch) -> (rd.S.rd_var, rd.S.rd_poly_var, sch))
        defs) in
  let (env, defs) =
    List.fold_left_map
      (finalize_rec_def ~evs ~cs ~rec_ctx) env defs in
  (env, defs)

and tr_let_rec_defs env targs named defs =
  match targs with
  | [] ->
    let (env, named) = tr_named_params env named in
    let env = Env.enter_rec_context env in
    let (env, defs) = List.fold_left_map prepare_rec_def env defs in
    let defs = List.map (check_rec_def env [] named) defs in
    ([], named, defs)
  
  | _ ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, targs) = Env.add_named_tvars env targs in
    let (env, named) = tr_named_params env named in
    let env = Env.enter_rec_context env in
    let (env, defs) = List.fold_left_map prepare_rec_def env defs in
    let defs = List.map (check_rec_def env targs named) defs in
    ConstrSolver.leave_scope_with_schemes ~env0 ~tvars:(List.map snd targs)
      (Env.constraints env)
      (List.map (fun (_, _, sch, _) -> sch) defs);
    (targs, named, defs)

and prepare_rec_def env (rd : S.rec_def) =
  let sch = Type.tr_scheme_expr env rd.rd_scheme in
  let env = Env.add_rec_var env rd.rd_var sch in
  (env, (rd, sch))

and check_rec_def env targs named ((rd : S.rec_def), sch) =
  let body = check_scheme env rd.rd_body sch in
  let (body, poly_sch) = ExprUtils.generalize targs named body sch in
  (rd, body, poly_sch, sch)

and finalize_rec_def ~evs ~cs ~rec_ctx env (rd, body, poly_sch, _) =
  let env = Env.add_lpoly_var env rd.S.rd_poly_var evs cs poly_sch in
  let def =
    { T.rd_var    = rd.S.rd_poly_var;
      T.rd_body   =
        ExprUtils.generalize_constr evs cs
          (ExprUtils.update_rec_body ~rec_ctx body);
      T.rd_evars  = evs;
      T.rd_constr = cs;
      T.rd_scheme = poly_sch
    } in
  (env, def)

(* ========================================================================= *)
(** Check if given polymorphic expression has a given scheme. *)
and check_scheme env (e : S.poly_fun) (sch : T.scheme) =
  match e.data with
  | PF_Fun([], xs, body) ->
    assert (List.is_empty sch.sch_targs);
    assert (List.length xs = List.length sch.sch_named);
    let (env, xs) =
      List.fold_left_map
        (fun env (x, (_, sch)) -> (Env.add_poly_var env x sch, (x, sch)))
        env
        (List.combine xs sch.sch_named) in
    let (body, Checked) = check_type env body sch.sch_body (Check Pure) in
    ExprUtils.mk_fns xs body

  | PF_Fun(tvars, xs, body) ->
    let env0 = env in
    let env = Env.enter_scope env in
    assert (List.length tvars = List.length sch.sch_targs);
    let ((env, sub), tvars) =
      List.fold_left_map
        (fun (env, sub) (x, (_, y)) ->
          let (env, z) = Env.add_tvar env x in
          ((env, T.Subst.rename sub y z), z))
        (env, T.Subst.empty)
        (List.combine tvars sch.sch_targs) in
    assert (List.length xs = List.length sch.sch_named);
    let (env, xs) =
      List.fold_left_map
        (fun env (x, (_, sch)) ->
          let sch = T.Scheme.subst sub sch in
          (Env.add_poly_var env x sch, (x, sch)))
        env
        (List.combine xs sch.sch_named) in
    let (body, Checked) =
      check_type env body (T.Type.subst sub sch.sch_body) (Check Pure) in
    ConstrSolver.leave_scope ~env0 ~tvars (Env.constraints env);
    ExprUtils.mk_tfuns tvars (ExprUtils.mk_fns xs body)

  | PF_Hole hole ->
    begin match BRef.get hole with
    | Some e -> check_scheme env e sch
    | None ->
      (* Method constraints should be already solved. *)
      assert false
    end

(* ========================================================================= *)
and infer_type : type ed.
  Env.t -> S.expr -> (T.ceffect, ed) request ->
    T.expr * T.typ * (T.ceffect, ed) response =
  fun env e eff_req ->
  match e.data with
  | EInst(e1, tps, es) ->
    let (e1, sch) = infer_scheme env e1 in
    let tps = List.map (Type.tr_type_expr env) tps in
    assert (List.length sch.sch_targs = List.length tps);
    let sub = T.Subst.for_named_tvars sch.sch_targs tps in
    let schs =
      List.map (fun (_, sch) -> T.Scheme.subst sub sch) sch.sch_named in
    assert (List.length schs = List.length es);
    let es = List.map2 (check_scheme env) es schs in
    ( ExprUtils.mk_inst e1 tps es,
      T.Type.subst sub sch.sch_body,
      return_pure eff_req )

  | ENum n ->
    let tp = T.Type.t_var (T.BuiltinType.tv_int) in
    (T.ENum n, tp, return_pure eff_req)

  | ENum64 n ->
    let tp = T.Type.t_var (T.BuiltinType.tv_int64) in
    (T.ENum64 n, tp, return_pure eff_req)

  | EStr s ->
    let tp = T.Type.t_var (T.BuiltinType.tv_string) in
    (T.EStr s, tp, return_pure eff_req)

  | EChr c ->
    let tp = T.Type.t_var (T.BuiltinType.tv_char) in
    (T.EChr c, tp, return_pure eff_req)

  | EFn(x, sch, body, _) ->
    let sch = Type.tr_scheme_expr env sch in
    let env = Env.add_poly_var env x sch in
    let (body, tp, Infered eff) = infer_type env body Infer in
    let res = T.EFn(x, sch, body) in
    let res_tp = T.Type.t_arrow sch tp eff in
    (res, res_tp, return_pure eff_req)

  | EAppPoly(e1, e2) ->
    let (e1, ftp, eff_resp1) = infer_type env e1 eff_req in
    let (sch, tp, eff) = Subtyping.as_arrow ftp in
    let e2 = check_scheme env e2 sch in
    let eff_resp2 = return_effect env ~node:e eff_req eff in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2 ] in
    (T.EApp(e1, e2), tp, eff_resp)

  | EAppMono(e1, e2) ->
    let (e1, ftp, eff_resp1) = infer_type env e1 eff_req in
    let (sch, tp, eff) = Subtyping.as_arrow ftp in
    assert (T.Scheme.is_monomorphic sch);
    let (e2, eff_resp2) = check_type env e2 sch.sch_body eff_req in
    let eff_resp3 = return_effect env ~node:e eff_req eff in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2; eff_resp3 ] in
    (T.EApp(e1, e2), tp, eff_resp)

  | ELetPoly(x, e1, e2) ->
    let (env, e1) = tr_let_poly env x e1 in
    let (e2, tp, eff_resp) = infer_type env e2 eff_req in
    (T.ELetPure(x, e1, e2), tp, eff_resp)

  | ELetMono(x, e1, e2) ->
    let (e1, tp1, eff_resp1) = infer_type env e1 eff_req in
    let env = Env.add_mono_var env x tp1 in
    let (e2, tp2, eff_resp2) = infer_type env e2 eff_req in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2 ] in
    (T.ELet(x, e1, e2), tp2, eff_resp)

  | ELetRec { targs; named; defs; body } ->
    begin match defs with
    | [] -> infer_type env body eff_req
    | _  ->
      let (env, defs) = tr_let_rec env targs named defs in
      let (body, tp, eff_resp) = infer_type env body eff_req in
      (T.ELetRec(defs, body), tp, eff_resp)
    end

  | ERecCtx e2 ->
    let env = Env.commit_rec_context env in
    let (e2, tp, eff_resp1) = infer_type env e2 eff_req in
    let eff_resp2 = return_effect env ~node:e eff_req (Impure T.Effct.pure) in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2 ] in
    (T.ERecCtx e2, tp, eff_resp)

  | EData(dds, e2) ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, dds, tvars) = DataType.tr_data_defs env dds in
    let (e2, tp, eff_resp) = infer_type env e2 eff_req in
    let res_tp = Subtyping.type_shape env0 tp in
    let eff = bidir_result eff_req eff_resp in
    let origin = ODataScope(e.pos, e.pp, tp, eff) in
    Subtyping.subtype ~origin env tp res_tp;
    Subtyping.subceffect ~origin env eff (Impure (Env.fresh_gvar env0));
    ConstrSolver.leave_scope_with_type_eff ~env0 ~tvars
      (Env.constraints env) res_tp eff;
    (T.EData(dds, e2), res_tp, eff_resp)

  | ETypeAlias(a, tp, e2) ->
    let tp  = Type.tr_type_expr env tp in
    let env = Env.add_type_alias env a tp in
    infer_type env e2 eff_req

  | EMatchEmpty(prf, e1, tp, eff) ->
    let (prf, tp1, ctors, match_eff) = ProofExpr.tr_proof_expr env prf in
    assert (List.is_empty ctors);
    let (e1, eff_resp1) = check_type env e1 tp1 eff_req in
    let tp = Type.tr_type env tp in
    let (eff, eff_resp2) = effect_annot env eff eff_req in
    let eff_resp3 = return_effect env ~node:e eff_req match_eff in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2; eff_resp3 ] in
    let res = T.EMatch(prf, e1, [], tp, eff) in
    (res, tp, eff_resp)

  | EMatch(e1, cls, tp, eff) ->
    let (e1, tp1, eff_resp1) = infer_type env e1 eff_req in
    let tp = Type.tr_type env tp in
    let (eff, eff_resp2) = effect_annot env eff eff_req in
    let cls = List.map (tr_match_clause env tp1 tp eff) cls in
    let res = PatternMatch.tr_match ~pos:e.pos e1 ~tp ~eff cls in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2 ] in
    (res, tp, eff_resp)

  | EMatchPoly(e1, pat, e2, tp, eff) ->
    let pat_pos = pat.pos in
    let (e1, sch) = infer_scheme env e1 in
    let (pat, penv) = Pattern.check_scheme env pat sch in
    let tp = Type.tr_type env tp in
    let (eff, eff_resp) = effect_annot env eff eff_req in
    let cl = tr_match_clause_body ~pos:pat_pos env pat penv e2 tp eff in
    let res = PatternMatch.tr_match ~pos:e.pos e1 ~tp ~eff [cl] in
    (res, tp, eff_resp)

  | EHandle(eff_var, x, e1, e2) ->
    let (e1, tp1, eff_resp1) = infer_type env e1 eff_req in
    let (a, cap_tp, in_tp, in_eff, out_tp, out_eff) =
      Subtyping.as_handler tp1 in
    let eff_resp2 = return_effect env ~node:e eff_req (Impure out_eff) in
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, eff_var) = Env.add_tvar env eff_var in
    let sub = T.Subst.rename T.Subst.empty a eff_var in
    let cap_tp = T.Type.subst sub cap_tp in
    let in_tp  = T.Type.subst sub in_tp in
    let in_eff = T.Effct.subst sub in_eff in
    let env = Env.add_mono_var env x cap_tp in
    let (e2, Checked) = check_type env e2 in_tp (Check (Impure in_eff)) in
    ConstrSolver.leave_scope ~env0 ~tvars:[eff_var] (Env.constraints env);
    let res = ExprUtils.mk_handle eff_var x cap_tp e1 e2 in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2 ] in
    (res, out_tp, eff_resp)

  | EHandler h ->
    (* create effect variable *)
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, eff_var) = Env.add_tvar env h.eff_var in
    (* Compute types and effects *)
    let delim_tp  = Type.tr_type env h.delim_tp in
    let delim_eff = Env.fresh_gvar env0 in
    let cap_tp    = Type.tr_type env h.cap_type in
    let in_tp     = Type.tr_type env h.body_tp in
    let out_eff   = T.Effct.join (Env.fresh_gvar env0) delim_eff in
    (* Add label *)
    let h_eff = T.Effct.var eff_var in
    let env = Env.add_mono_var env h.label
      (T.Type.t_label h_eff delim_tp delim_eff) in
    (* Translate the capability *)
    let (cap_body, Checked) = check_type env h.cap_body cap_tp (Check Pure) in
    (* Translate the return clause *)
    let ret_env = Env.add_mono_var env h.ret_var in_tp in
    let (ret_body, Checked) =
      check_type ret_env h.ret_body delim_tp (Check (Impure delim_eff)) in
    (* Translate the finally clause *)
    let fin_env = Env.add_mono_var env h.fin_var delim_tp in
    let (fin_body, fin_tp, Checked) =
      infer_type fin_env h.fin_body (Check (Impure out_eff)) in
    (* Compute output type *)
    let out_tp  = Subtyping.type_shape env0 fin_tp in
    let origin =
      OHandlerScope(h.fin_body.pos, h.fin_body.pp, eff_var, fin_tp)
    in
    Subtyping.subtype ~origin env fin_tp out_tp;
    (* Build the result type *)
    let in_eff = T.Effct.join h_eff delim_eff in
    let tp = T.Type.t_handler eff_var cap_tp in_tp in_eff out_tp out_eff in
    (* Leave the scope, and return the result *)
    ConstrSolver.leave_scope_with_type_eff ~env0 ~tvars:[eff_var]
      (Env.constraints env) tp Pure;
    let res =
      ExprUtils.mk_handler
        ~eff_var ~lbl_var:h.label
        ~delim_tp ~delim_eff ~cap_tp ~in_tp ~in_eff 
        ~cap_body ~ret_var:h.ret_var ~ret_body ~fin_var:h.fin_var ~fin_body
        () in
    (res, tp, return_pure eff_req)

  | EEffect(lbl, x, body, res_tp) ->
    let (lbl, lbl_tp, eff_resp1) = infer_type env lbl eff_req in
    let (eff, delim_tp, delim_eff) = Subtyping.as_label lbl_tp in
    let res_tp = Type.tr_type env res_tp in
    let cont_tp =
      T.Type.t_arrow (T.Scheme.of_type res_tp) delim_tp (Impure delim_eff) in
    let env = Env.add_mono_var env x cont_tp in
    let (body, Checked) =
      check_type env body delim_tp (Check (Impure delim_eff)) in
    let eff_resp2 = return_effect env ~node:e eff_req (Impure eff) in
    let eff_resp = eff_resp_join eff_resp1 [ eff_resp2 ] in
    let res = T.EShift(lbl, x, body, res_tp) in
    (res, res_tp, eff_resp)

  | EExtern(name, tp) ->
    let tp = Type.tr_type env tp in
    (T.EExtern(name, tp), tp, return_pure eff_req)

  | EAnnot(e1, tp) ->
    let tp = Type.tr_type_expr env tp in
    let (e1, eff_resp) = check_type env e1 tp eff_req in
    (e1, tp, eff_resp)

  | ERepl(func, tp) ->
    let tp = Type.tr_type env tp in
    let (eff, eff_resp) = effect_annot env Impure eff_req in
    let func () =
      let (body, Checked) = check_type env (func ()) tp (Check eff) in
      ConstrSolver.final_solve env;
      body
    in
    (T.ERepl(func, tp, eff), tp, eff_resp)

  | EReplExpr { body; to_str; rest } ->
    (* Infer type of the body, and check that to_str can convert it to
      string *)
    let (e1, tp1, eff_resp1) = infer_type env body eff_req in
    let (to_str_expr, to_str_tp, eff_resp2) = infer_type env to_str eff_req in
    let (sch, str_tp, to_str_eff) = Subtyping.as_arrow to_str_tp in
    assert (T.Scheme.is_monomorphic sch);
    begin match T.Type.view str_tp with
    | TVar tv when T.TVar.equal tv (T.BuiltinType.tv_string) -> ()
    | _ -> assert false
    end;
    Subtyping.subtype
      ~origin:(OExprType(to_str.pos, to_str.pp, tp1, sch.sch_body))
      env tp1 sch.sch_body;
    let eff_resp3 = return_effect env ~node:e eff_req to_str_eff in
    (* Infer type of the rest expression *)
    let (rest, rest_tp, eff_resp4) = infer_type env rest eff_req in
    (* Pretty-print the type of the body *)
    let ctx = T.Pretty.empty_context () in
    let tp1 = T.Pretty.pp_type ctx body.pp tp1 in
    (* Build the result *)
    let res = T.EReplExpr(T.EApp(to_str_expr, e1), tp1, rest) in
    let eff_resp =
      eff_resp_join eff_resp1 [ eff_resp2; eff_resp3; eff_resp4 ] in
    (res, rest_tp, eff_resp)

   | EReplDir { cont ; rest }-> 
      let (rest, rest_tp, eff_resp4) = infer_type env rest eff_req in
      let res = T.EReplDir (cont , rest) in
      (res , rest_tp , eff_resp4)
   



and check_type : type ed.
  Env.t -> S.expr -> T.typ -> (T.ceffect, ed) request ->
    T.expr * (T.ceffect, ed) response =
  fun env e tp eff_req ->
  let (res, tp', eff_resp) = infer_type env e eff_req in
  let origin = OExprType(e.pos, e.pp, tp', tp) in
  Subtyping.subtype ~origin env tp' tp;
  (res, eff_resp)

(* ========================================================================= *)
(** Translate a match clause. *)
and tr_match_clause env pat_tp res_tp res_eff ((pat, body) : S.match_clause) =
  let pat_pos = pat.pos in
  let (pat, penv) = Pattern.check_type env pat pat_tp in
  tr_match_clause_body ~pos:pat_pos env pat penv body res_tp res_eff

(** Translate a body of a match clause. *)
and tr_match_clause_body ~pos env pat penv body tp eff =
  if Pattern.PEnv.has_existential penv then begin
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, tvars, vars) = Pattern.PEnv.open_penv env penv in
    let (body, Checked) = check_type env body tp (Check eff) in
    ConstrSolver.leave_scope ~env0 ~tvars (Env.constraints env);
    { PatternMatch.cl_pos   = pos;
      PatternMatch.cl_pat   = pat;
      PatternMatch.cl_body  = ExprUtils.mk_clause_body tvars vars body;
      PatternMatch.cl_tvars = Pattern.PEnv.tvars penv;
      PatternMatch.cl_vars  = Pattern.PEnv.vars penv
    }
  end else begin
    let (env, tvs, vars) = Pattern.PEnv.open_penv env penv in
    assert (List.is_empty tvs);
    let (body, Checked) = check_type env body tp (Check eff) in
    { PatternMatch.cl_pos   = pos;
      PatternMatch.cl_pat   = pat;
      PatternMatch.cl_body  = ExprUtils.mk_clause_body tvs vars body;
      PatternMatch.cl_tvars = Pattern.PEnv.tvars penv;
      PatternMatch.cl_vars  = Pattern.PEnv.vars penv
    }
  end
