(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

open Common
open TypeCheckFix

(* ------------------------------------------------------------------------- *)
(** Bidirectional type-checker of expressions *)
let tr_expr : type dir. tcfix:tcfix ->
  Env.t -> S.expr -> (T.typ, dir) request -> dir expr_result =
  fun ~tcfix env e tp_req ->
  let open (val tcfix : TCFix) in
  match tp_req with
  | Infer    -> infer_expr_type env e
  | Check tp -> check_expr_type env e tp

(* ------------------------------------------------------------------------- *)
let infer_expr_type ~tcfix ?app_type env (e : S.expr) =
  let open (val tcfix : TCFix) in
  let make data = { e with data = data } in
  let pos = e.pos in
  match e.data with
  | EMatch _ | EEffect _ | EExtern _ | ERepl _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let er = check_expr_type env e tp in
    { er with er_type = Infered tp }

  | EUnit ->
    { er_expr   = make (T.ECtor(make T.EUnitPrf, 0, [], [], []));
      er_type   = Infered T.Type.t_unit;
      er_effect = Pure;
      er_constr = []
    }

  | ENum n ->
    { er_expr   = make (T.ENum n);
      er_type   = Infered (T.Type.t_var T.BuiltinType.tv_int);
      er_effect = Pure;
      er_constr = []
    }

  | ENum64 n ->
    { er_expr   = make (T.ENum64 n);
      er_type   = Infered (T.Type.t_var T.BuiltinType.tv_int64);
      er_effect = Pure;
      er_constr = []
    }

  | EStr s ->
    { er_expr   = make (T.EStr s);
      er_type   = Infered (T.Type.t_var T.BuiltinType.tv_string);
      er_effect = Pure;
      er_constr = []
    }

  | EChr c ->
    { er_expr   = make (T.EChr c);
      er_type   = Infered (T.Type.t_var T.BuiltinType.tv_char);
      er_effect = Pure;
      er_constr = []
    }

  | EPoly(e, inst) ->
    let (p_ctx, e, sch) = PolyExpr.infer_use_scheme ~tcfix ?app_type env e in
    PolyExpr.plug_inst_context p_ctx
      (Inst.instantiate_poly_expr ~tcfix ~pos env e sch inst)

  | EFn(pat, body) ->
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    let (env, pat, sch, eff1) = Pattern.infer_scheme_ext env pat in
    let er_body = check_expr_type env body tp2 in
    let eff = T.Effect.join eff1 er_body.er_effect in
    let (x, body) = ExprUtils.match_var pat er_body.er_expr tp2 eff in
    { er_expr   = make (T.EFn(x, sch, body, eff));
      er_type   = Infered (T.Type.t_arrow sch tp2 eff);
      er_effect = Pure;
      er_constr = er_body.er_constr
    }

  | EApp(e1, e2) ->
    let er1 = infer_expr_type ?app_type env e1 in
    let ftp = expr_result_type er1 in
    begin match Unification.to_arrow env ftp with
    | Arr_UVar -> assert false
    | Arr_Arrow(sch, tp2, eff) ->
      begin match PolyExpr.check_def_scheme ~tcfix env e2 sch with
      | Mono er2 ->
        { er_expr   = make (T.EAppMono(er1.er_expr, er2.er_expr));
          er_type   = Infered tp2;
          er_effect = T.Effect.joins [er1.er_effect; er2.er_effect; eff];
          er_constr = er1.er_constr @ er2.er_constr
        }
      | Poly(e2, cs2) ->
        { er_expr   = make (T.EAppPoly(er1.er_expr, e2));
          er_type   = Infered tp2;
          er_effect = T.Effect.joins [er1.er_effect; eff];
          er_constr = er1.er_constr @ cs2
        }
      end
    | Arr_No ->
      Error.fatal (Error.expr_not_function ~pos:e1.pos ~env ftp)
    end

  | EDefs(defs, e) ->
    check_defs env ParameterEnv.empty defs Infer
      { run = fun env _ req -> tr_expr ~tcfix env e req }

  | EHandler(cap, rcs, fcs) ->
    let fin_tp = Env.fresh_uvar env T.Kind.k_type in
    (* TODO: effect and label could be named here *)
    let (env, a) = Env.add_anon_tvar ~pos ~name:"E" env T.Kind.k_effect in
    let delim_tp = Env.fresh_uvar env T.Kind.k_type in
    let (env, lx) = Env.add_the_label env (T.Type.t_label delim_tp) in
    let er_cap = infer_expr_type env cap in
    begin match er_cap.er_effect with
    | Pure -> ()
    | Impure -> Error.report (Error.impure_handler ~pos)
    end;
    let cap_tp = expr_result_type er_cap in
    let body_tp = Env.fresh_uvar env T.Kind.k_type in
    let (ret_x, er_ret) =
      MatchClause.tr_return_clauses ~tcfix ~pos env body_tp rcs
        (Check delim_tp) in
    let (fin_x, er_fin) =
      MatchClause.tr_finally_clauses ~tcfix ~pos env delim_tp fcs
        (Check fin_tp) in
    { er_expr   = make (T.EHandler {
          label    = lx;
          effect   = a;
          delim_tp = delim_tp;
          cap_type = cap_tp;
          cap_body = er_cap.er_expr;
          ret_var  = ret_x;
          body_tp  = body_tp;
          ret_body = er_ret.er_expr;
          fin_var  = fin_x;
          fin_body = er_fin.er_expr;
        });
      er_type   = Infered (T.Type.t_handler a cap_tp body_tp fin_tp);
      er_effect = Pure;
      er_constr = er_cap.er_constr @ er_ret.er_constr @ er_fin.er_constr
    }

  | EAnnot(e, tp) ->
    let tp_expr = Type.tr_ttype env tp in
    let tp = T.TypeExpr.to_type tp_expr in
    let eff = make (T.TE_Type T.Type.t_effect) in
    let er = check_expr_type env e tp in
    { er_expr = make (T.EAnnot(er.er_expr, tp_expr, eff));
      er_type = Infered tp;
      er_effect = er.er_effect;
      er_constr = er.er_constr
    }
(* ------------------------------------------------------------------------- *)
(** Infer the type of optional label *)
let check_label ~tcfix ~pos env lbl_opt =
  let (lbl, lbl_tp, cs) =
    match lbl_opt with
    | Some lbl ->
      let er = infer_expr_type ~tcfix env lbl in
      (er.er_expr, expr_result_type er, er.er_constr)
    | None ->
      begin match Env.lookup_the_label env with
      | None -> Error.fatal (Error.unbound_the_label ~pos)
      | Some(x, sch, on_use) ->
        on_use pos;
        let lbl = { T.pos = pos; T.data = T.EVar x } in
        ParamResolve.instantiate ~pos env ParamResolve.no_reinst lbl sch
      end
  in
  let delim_tp =
    match Unification.to_label env lbl_tp with
    | L_Label delim_tp -> delim_tp
    | L_No ->
      begin match lbl_opt with
      | Some _ ->
        Error.report (Error.expr_not_label ~pos:lbl.pos ~env lbl_tp)
      | None ->
        Error.report (Error.wrong_label_type ~pos ~env lbl_tp)
      end;
      Env.fresh_uvar env T.Kind.k_type
  in
  (lbl, delim_tp, cs)
      
(* ------------------------------------------------------------------------- *)
(** Check the sequence of REPL definitions, provided by a user. Always
  in type-check mode. *)
let rec check_repl_def_seq ~tcfix env penv def_seq tp =
  let open (val tcfix : TCFix) in
  let func () =
    match def_seq () with
    | Seq.Nil -> assert false
    | Seq.Cons(defs, def_seq) ->
      let cont (type dir) env penv (tp_req : (_, dir) request) :
          dir expr_result =
        let (tp, tp_resp) : _ * (_, dir) response =
          match tp_req with
          | Check tp -> (tp, Checked)
          | Infer    ->
            let tp = Env.fresh_uvar env T.Kind.k_type in
            (tp, Infered tp)
        in
        { er_expr   = check_repl_def_seq ~tcfix env penv def_seq tp;
          er_type   = tp_resp;
          er_effect = Impure;
          er_constr = []
        }
      in
      let er = check_defs env penv defs (Check tp) { run = cont } in
      Constr.solve_all er.er_constr;
      er.er_expr
  in
  { T.pos  = Position.nowhere;
    T.data = T.ERepl(InterpLib.Error.wrap_repl_cont func, tp)
  }

(* ------------------------------------------------------------------------- *)
(** Default action in type-check mode: switching to infer mode *)
let check_expr_type_default ~tcfix env (e : S.expr) tp =
  let er = infer_expr_type ~tcfix ~app_type:tp env e in
  let tp' = expr_result_type er in
  Error.check_unify_result ~pos:e.pos
    (Unification.subtype env tp' tp)
    ~on_error:(Error.expr_type_mismatch ~env tp' tp);
  { er with er_type = Checked }

(* ------------------------------------------------------------------------- *)
let check_expr_type ~tcfix env (e : S.expr) tp =
  let open (val tcfix : TCFix) in
  let make data = { e with data = data } in
  let pos = e.pos in
  match e.data with
  | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EPoly _ | EApp _
  | EAnnot _ ->
    check_expr_type_default ~tcfix env e tp

  | EFn(pat, body) ->
    begin match Unification.from_arrow env tp with
    | Arr_UVar ->
      check_expr_type_default ~tcfix env e tp
    | Arr_Arrow(sch, tp2, eff) ->
      let (env, pat, pat_eff) = Pattern.check_scheme_ext env pat sch in
      let er_body = check_expr_type env body tp2 in
      let fun_eff = T.Effect.join pat_eff er_body.er_effect in
      begin match fun_eff, eff with
      | Impure, Pure -> Error.report (Error.func_not_pure ~pos)
      | Pure, _ | _, Impure -> ()
      end;
      let (x, body) = ExprUtils.match_var pat er_body.er_expr tp2 eff in
      { er_expr   = make (T.EFn(x, sch, body, eff));
        er_type   = Checked;
        er_effect = Pure;
        er_constr = er_body.er_constr
      }
    | Arr_No ->
      Error.report (Error.expr_not_function_ctx ~pos ~env tp);
      let er = infer_expr_type env e in
      { er with er_type = Checked }
    end

  | EDefs(defs, e) ->
    check_defs env ParameterEnv.empty defs (Check tp)
      { run = fun env _ req -> tr_expr ~tcfix env e req }

  | EMatch(me, []) ->
    let er = infer_expr_type env me in
    let mtp = expr_result_type er in
    begin match T.Type.whnf mtp with
    | Whnf_Neutral(NH_Var x, targs_rev) ->
      begin match Env.lookup_adt env x with
      | Some { adt_ctors = []; adt_proof; adt_effect; _ } ->
        let targs =
          List.rev_map (fun tp -> make_nowhere (T.TE_Type tp)) targs_rev in
        let proof = make_nowhere (T.EInst(adt_proof, targs, [])) in
        let eff = T.Effect.join er.er_effect adt_effect in
        { er_expr   = make (T.EMatchEmpty(proof, er.er_expr, tp, eff));
          er_type   = Checked;
          er_effect = eff;
          er_constr = er.er_constr
        }

      | Some { adt_ctors = _ :: _; _ } ->
        Error.fatal (Error.empty_match_on_nonempty_adt ~pos ~env mtp)
      | None ->
        Error.fatal (Error.empty_match_on_non_adt ~pos ~env mtp)
      end

    | Whnf_Neutral(NH_UVar _, _) | Whnf_Arrow _ | Whnf_Handler _
    | Whnf_Label _ ->
      Error.fatal (Error.empty_match_on_non_adt ~pos ~env mtp)

    | Whnf_Effect ->
      failwith "Internal kind error"
    end

  | EMatch(e, cls) ->
    let er = infer_expr_type env e in
    let mtp = expr_result_type er in
    let (cls, meff, cs) =
      MatchClause.check_match_clauses ~tcfix env mtp cls tp in
    let eff = T.Effect.join er.er_effect meff in
    { er_expr   = make (T.EMatch(er.er_expr, cls, tp, eff));
      er_type   = Checked;
      er_effect = eff;
      er_constr = er.er_constr @ cs
    }

  | EHandler(cap, rcs, fcs) ->
    begin match Unification.from_handler env tp with
    | H_Handler(b, cap_tp, tp_in, tp_out) ->
      (* TODO: effect and label could be named here *)
      let (env, a) = Env.add_anon_tvar ~pos ~name:"E" env T.Kind.k_effect in
      let sub    = T.Subst.rename_to_fresh T.Subst.empty b a in
      let cap_tp = T.Type.subst sub cap_tp in
      let tp_in  = T.Type.subst sub tp_in in
      let delim_tp = Env.fresh_uvar env T.Kind.k_type in
      let (env, lx) = Env.add_the_label env (T.Type.t_label delim_tp) in
      let er_cap = check_expr_type env cap cap_tp in
      begin match er_cap.er_effect with
      | Pure -> ()
      | Impure -> Error.report (Error.impure_handler ~pos)
      end;
      let (ret_x, er_ret) =
        MatchClause.tr_return_clauses ~tcfix ~pos env tp_in rcs
          (Check delim_tp) in
      let (fin_x, er_fin) =
        MatchClause.tr_finally_clauses ~tcfix ~pos env delim_tp fcs
          (Check tp_out) in
      { er_expr   = make (T.EHandler {
            label    = lx;
            effect   = a;
            delim_tp = delim_tp;
            cap_type = cap_tp;
            cap_body = er_cap.er_expr;
            ret_var  = ret_x;
            body_tp  = tp_in;
            ret_body = er_ret.er_expr;
            fin_var  = fin_x;
            fin_body = er_fin.er_expr;
          });
        er_type   = Checked;
        er_effect = Pure;
        er_constr = er_cap.er_constr @ er_ret.er_constr @ er_fin.er_constr
      }
    | H_No ->
      Error.fatal (Error.expr_not_handler_ctx ~pos ~env tp)
    end

  | EEffect(lbl_opt, cont_pat, body) ->
    let (lbl, delim_tp, lbl_cs) = check_label ~tcfix ~pos env lbl_opt in
    let cont_tp = T.Type.t_arrow (T.Scheme.of_type tp) delim_tp Impure in
    let (env, cont_pat, _) = Pattern.check_type_ext env cont_pat cont_tp in
    let er_body = check_expr_type env body delim_tp in
    let (x, body) =
      ExprUtils.match_var cont_pat er_body.er_expr delim_tp Impure in
    { er_expr   = make (T.EEffect(lbl, x, body, tp));
      er_type   = Checked;
      er_effect = Impure;
      er_constr = lbl_cs @ er_body.er_constr
    }

  | EExtern name ->
    { er_expr   = make (T.EExtern(name, tp));
      er_type   = Checked;
      er_effect = Pure;
      er_constr = []
    }

  | ERepl def_seq ->
    { er_expr   = check_repl_def_seq ~tcfix env ParameterEnv.empty def_seq tp;
      er_type   = Checked;
      er_effect = Impure;
      er_constr = []
    }
