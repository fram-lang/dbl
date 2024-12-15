(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

open Common
open TypeCheckFix

(* ------------------------------------------------------------------------- *)
(** Default behaviour of type-inferece for application *)
let infer_app_type ~tcfix ~pos env e1 e2 eff =
  let open (val tcfix : TCFix) in
  let make data = T.{ data; pos } in
  let (e1, ftp, r_eff1) = infer_expr_type env e1 eff in
  match Unification.to_arrow env ftp with
  | Arr_UVar -> assert false
  | Arr_Pure(sch, vtp) ->
    let (e2, r_eff2) = PolyExpr.check_actual_arg ~tcfix env e2 sch eff in
    (make (T.EApp(e1, e2)), vtp, ret_effect_join r_eff1 r_eff2)
  | Arr_Impure(sch, vtp, f_eff) ->
    let (e2, r_eff2) = PolyExpr.check_actual_arg ~tcfix env e2 sch eff in
    Error.check_unify_result ~pos:e1.pos
      (Unification.subeffect env f_eff eff)
      ~on_error:(Error.func_effect_mismatch ~env f_eff eff);
    (make (T.EApp(e1, e2)), vtp, Impure)
  | Arr_No ->
    Error.fatal (Error.expr_not_function ~pos:e1.pos ~env ftp)

(* ------------------------------------------------------------------------- *)
(** Bidirectional type-checker of expressions *)
let tr_expr : type dir. tcfix:tcfix ->
  Env.t -> S.expr -> (T.typ, dir) request -> T.effrow ->
    (T.expr * (T.typ, dir) response * ret_effect) =
  fun ~tcfix env e tp_req eff ->
  let open (val tcfix : TCFix) in
  match tp_req with
  | Infer ->
    let (e, tp, r_eff) = infer_expr_type env e eff in
    (e, Infered tp, r_eff)
  | Check tp ->
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, Checked, r_eff)

(* ------------------------------------------------------------------------- *)
let infer_expr_type ~tcfix env (e : S.expr) eff =
  let open (val tcfix : TCFix) in
  let make data = { e with data = data } in
  let pos = e.pos in
  match e.data with
  | EMatch _ | EEffect _ | EExtern _ | ERepl _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, tp, r_eff)

  | EUnit ->
    (make (T.ECtor(make T.EUnitPrf, 0, [], [])), T.Type.t_unit, Pure)

  | ENum n ->
    (make (T.ENum n), T.Type.t_var T.BuiltinType.tv_int, Pure)

  | ENum64 n ->
    (make (T.ENum64 n), T.Type.t_var T.BuiltinType.tv_int64, Pure)

  | EStr s ->
    (make (T.EStr s), T.Type.t_var T.BuiltinType.tv_string, Pure)

  | EChr c ->
    (make (T.EChr c), T.Type.t_var T.BuiltinType.tv_char, Pure)

  | EPoly(e, tinst, inst) ->
    let (p_ctx, e, sch, hints1) = PolyExpr.infer_scheme ~tcfix env e eff in
    Uniqueness.check_type_inst_uniqueness tinst;
    Uniqueness.check_inst_uniqueness inst;
    let tinst = Type.check_type_insts env tinst sch.sch_targs in
    let (hints2, cached_inst) =
      TypeHints.extract_implicit_type_hints ~tcfix ~pos env sch inst eff in
    let hints = TypeHints.merge_hints hints1 hints2 in
    let (sub, tps) =
      ExprUtils.guess_types ~pos ~tinst ~hints env sch.sch_targs in
    let e = ExprUtils.make_tapp e tps in
    let named = List.map (T.NamedScheme.subst sub) sch.sch_named in
    let (i_ctx, inst, r_eff) =
      PolyExpr.check_explicit_insts ~tcfix env named inst cached_inst eff in
    let e = ExprUtils.instantiate_named_params env e named inst in
    let tp = T.Type.subst sub sch.sch_body in
    (p_ctx (i_ctx e) tp r_eff)

  | EFn(arg, body) ->
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    let body_eff = Env.fresh_uvar env T.Kind.k_effrow in
    let (env, pat, sch, r_eff1) = Pattern.infer_arg_scheme env arg in
    let (body, r_eff2) = check_expr_type env body tp2 body_eff in
    let meff = match_effect (ret_effect_join r_eff1 r_eff2) body_eff in
    let (x, body) = ExprUtils.arg_match pat body tp2 meff in
    begin match ret_effect_join r_eff1 r_eff2 with
    | Pure ->
      let b = Unification.subeffect env body_eff T.Effect.pure in
      assert (b = Unify_Success);
      (make (T.EPureFn(x, sch, body)), T.Type.t_pure_arrow sch tp2, Pure)
    | Impure ->
      (make (T.EFn(x, sch, body)), T.Type.t_arrow sch tp2 body_eff, Pure)
    end

  | EApp(e1, e2) ->
    begin match e1.data with
    | EPoly({ data = EVar x; _ }, tinst, insts) ->
      begin match Env.lookup_var env x with
      | Some (VI_MethodFn name) ->
        let new_m = { e with data = S.EMethod(e2, name) } in
        let new_e = { e1 with data = S.EPoly(new_m, tinst, insts) } in
        infer_expr_type env new_e eff
      | _ -> infer_app_type ~tcfix ~pos env e1 e2 eff
      end
    | _ -> infer_app_type ~tcfix ~pos env e1 e2 eff
    end

  | EDefs(defs, e) ->
    let (e, Infered tp, r_eff) =
      check_defs env ImplicitEnv.empty defs Infer eff
        { run = fun env _ req eff -> tr_expr ~tcfix env e req eff }
    in
    (e, tp, r_eff)

  | EHandler(h, rcs, fcs) ->
    (* TODO: effect and label could be named here *)
    let (env, a) = Env.add_the_effect ~pos:e.pos env in
    let delim_tp  = Env.fresh_uvar env T.Kind.k_type in
    let delim_eff = Env.fresh_uvar env T.Kind.k_effrow in
    let (env, lx) =
      Env.add_the_label env (T.Type.t_var a) delim_tp delim_eff in
    let (h, tp, r_eff) = infer_expr_type env h T.Effect.pure in
    begin match r_eff with
    | Pure -> ()
    | Impure -> Error.report (Error.impure_handler ~pos:e.pos)
    end;
    let (ret_x, Infered body_tp, ret_body, Checked) =
      MatchClause.tr_opt_clauses ~tcfix ~pos:e.pos env Infer rcs
        (Check delim_tp) delim_eff
        ~on_error:(fun ~pos -> assert false) in
    let fin_eff = Env.fresh_uvar env T.Kind.k_effrow in
    let (fin_x, Checked, fin_body, Infered fin_tp) =
      MatchClause.tr_opt_clauses ~tcfix ~pos:e.pos env (Check delim_tp) fcs
        Infer fin_eff
        ~on_error:(fun ~pos -> assert false) in
    Error.check_unify_result ~pos
      (Unification.subeffect env delim_eff fin_eff)
      ~on_error:(Error.finally_effect_mismatch ~env delim_eff fin_eff);
    let e =
      make (T.EHandler {
        label     = lx;
        effect    = a;
        delim_tp  = delim_tp;
        delim_eff = delim_eff;
        cap_type  = tp;
        cap_body  = h;
        ret_var   = ret_x;
        body_tp   = body_tp;
        ret_body  = ret_body;
        fin_var   = fin_x;
        fin_body  = fin_body;
      }) in
    (e, T.Type.t_handler a tp body_tp delim_eff fin_tp fin_eff, Pure)

  | EAnnot(e, tp) ->
    let tp = Type.tr_ttype env tp in
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, tp, r_eff)

(* ------------------------------------------------------------------------- *)
(** Check the sequence of REPL definitions, provided by a user. Always
  in type-check mode. *)
let rec check_repl_def_seq ~tcfix env ienv def_seq tp eff =
  let open (val tcfix : TCFix) in
  let func () =
    match def_seq () with
    | Seq.Nil -> assert false
    | Seq.Cons(def, def_seq) ->
      let cont (type dir) env ienv (tp_req : (_, dir) request) eff :
          _ * (_, dir) response * _ =
        match tp_req with
        | Check tp ->
          let e = check_repl_def_seq ~tcfix env ienv def_seq tp eff in
          (e, Checked, Impure)
        | Infer ->
          let tp = Env.fresh_uvar env T.Kind.k_type in
          let e = check_repl_def_seq ~tcfix env ienv def_seq tp eff in
          (e, Infered tp, Impure)
      in
      let (e, Checked, _) =
        check_defs env ienv def (Check tp) eff { run = cont } in
      e
  in
  let e =
    { T.pos  = Position.nowhere;
      T.data = T.ERepl(InterpLib.Error.wrap_repl_cont func, tp, eff)
    } in
  e

(* ------------------------------------------------------------------------- *)
(** Default action in type-check mode: switching to infer mode *)
let check_expr_type_default ~tcfix env (e : S.expr) tp eff =
  let open (val tcfix : TCFix) in
  let pos = e.pos in
  let (e, tp', r_eff) = infer_expr_type env e eff in
  Error.check_unify_result ~pos
    (Unification.subtype env tp' tp)
    ~on_error:(Error.expr_type_mismatch ~env tp' tp);
  (e, r_eff)

(* ------------------------------------------------------------------------- *)
let check_expr_type ~tcfix env (e : S.expr) tp eff =
  let open (val tcfix : TCFix) in
  let make data = { e with data = data } in
  let pos = e.pos in
  match e.data with
  | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EPoly _ | EApp _ ->
    check_expr_type_default ~tcfix env e tp eff

  | EFn(arg, body) ->
    begin match Unification.from_arrow env tp with
    | Arr_UVar ->
      check_expr_type_default ~tcfix env e tp eff

    | Arr_Pure(sch, tp2) ->
      let (env, pat, r_eff1) = Pattern.check_arg_scheme env arg sch in
      let (body, r_eff2) = check_expr_type env body tp2 T.Effect.pure in
      if ret_effect_join r_eff1 r_eff2 <> Pure then
        Error.report (Error.func_not_pure ~pos);
      let (x, body) = ExprUtils.arg_match pat body tp2 None in
      (make (T.EPureFn(x, sch, body)), Pure)

    | Arr_Impure(sch, tp2, eff) ->
      let (env, pat, _) = Pattern.check_arg_scheme env arg sch in
      let (body, _) = check_expr_type env body tp2 eff in
      let (x, body) = ExprUtils.arg_match pat body tp2 (Some eff) in
      (make (T.EFn(x, sch, body)), Pure)

    | Arr_No ->
      Error.report (Error.expr_not_function_ctx ~pos ~env tp);
      let (e, _, r_eff) = infer_expr_type env e eff in
      (e, r_eff)
    end

  | EDefs(defs, e) ->
    let (e, Checked, r_eff) =
      check_defs env ImplicitEnv.empty defs (Check tp) eff
        { run = fun env _ req eff -> tr_expr ~tcfix env e req eff }
    in
    (e, r_eff)

  | EMatch(me, []) ->
    let (me, me_tp, r_eff1) = infer_expr_type env me eff in
    begin match T.Type.whnf me_tp with
    | Whnf_Neutral(NH_Var x, targs_rev) ->
      begin match Env.lookup_adt env x with
      | Some { adt_ctors = []; adt_proof; adt_strictly_positive; _ } ->
        let targs = List.rev targs_rev in
        let proof = ExprUtils.make_tapp adt_proof targs in
        let (meff, r_eff2) =
          if adt_strictly_positive then (None, Pure)
          else (Some eff, Impure)
        in
        (make (T.EMatchEmpty(proof, me, tp, meff)),
          ret_effect_join r_eff1 r_eff2)

      | Some { adt_ctors = _ :: _; _ } ->
        Error.fatal (Error.empty_match_on_nonempty_adt ~pos ~env me_tp)
      | None ->
        Error.fatal (Error.empty_match_on_non_adt ~pos ~env me_tp)
      end

    | Whnf_Neutral(NH_UVar _, _) | Whnf_PureArrow _
    | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
      Error.fatal (Error.empty_match_on_non_adt ~pos ~env me_tp)

    | Whnf_Effect _ | Whnf_Effrow _ ->
      failwith "Internal kind error"
    end

  | EMatch(e, cls) ->
    let (e, e_tp, r_eff1) = infer_expr_type env e eff in
    let (cls, r_eff2) =
      MatchClause.check_match_clauses ~tcfix env e_tp cls tp eff in
    (make (T.EMatch(e, cls, tp, match_effect r_eff2 eff)),
      ret_effect_join r_eff1 r_eff2)

  | EHandler(body, rcs, fcs) ->
    begin match Unification.from_handler env tp with
    | H_Handler(b, cap_tp, tp_in, eff_in, tp_out, eff_out) ->
      let (env, a) = Env.add_the_effect ~pos env in
      let sub    = T.Subst.rename_to_fresh T.Subst.empty b a in
      let cap_tp = T.Type.subst sub cap_tp in
      let tp_in  = T.Type.subst sub tp_in in
      let eff_in = T.Type.subst sub eff_in in
      let delim_tp  = Env.fresh_uvar env T.Kind.k_type in
      let delim_eff = Env.fresh_uvar env T.Kind.k_effrow in
      Error.check_unify_result ~pos
        (Unification.subeffect env eff_in (T.Effect.cons a delim_eff))
        ~on_error:(fun ~pos -> assert false);
      let (env, lx) =
        Env.add_the_label env (T.Type.t_var a) delim_tp delim_eff in
      let (body, r_eff) = check_expr_type env body cap_tp T.Effect.pure in
      begin match r_eff with
      | Pure -> ()
      | Impure -> Error.report (Error.impure_handler ~pos)
      end;
      let (ret_var, Checked, ret_body, Checked) =
        MatchClause.tr_opt_clauses ~tcfix ~pos env
          (Check tp_in) rcs (Check delim_tp) delim_eff
          ~on_error:(Error.return_type_mismatch ~env tp_in delim_tp) in
      Error.check_unify_result ~pos
        (Unification.subeffect env delim_eff eff_out)
        ~on_error:(Error.finally_effect_mismatch ~env delim_eff eff_out);
      let (fin_var, Checked, fin_body, Checked) =
        MatchClause.tr_opt_clauses ~tcfix ~pos env
          (Check delim_tp) fcs (Check tp_out) eff_out
          ~on_error:(Error.finally_type_mismatch ~env delim_tp tp_out) in
      let e =
        make (T.EHandler {
          label     = lx;
          effect    = a;
          delim_tp  = delim_tp;
          delim_eff = delim_eff;
          cap_type  = cap_tp;
          cap_body  = body;
          ret_var   = ret_var;
          body_tp   = tp_in;
          ret_body  = ret_body;
          fin_var   = fin_var;
          fin_body  = fin_body;
        }) in
      (e, Pure)

    | H_No ->
      Error.fatal (Error.expr_not_handler_ctx ~pos ~env tp)
    end

  | EEffect(arg, body) ->
    begin match Env.lookup_the_label env with
    | Some(lx, l_eff0, res_tp, res_eff) ->
      let l_eff = T.Type.t_closed_effrow (T.Type.effect_view l_eff0) in
      Error.check_unify_result ~pos
        (Unification.subeffect env l_eff eff)
        ~on_error:(Error.expr_effect_mismatch ~env l_eff eff);
      let r_tp = T.Type.t_arrow (T.Scheme.of_type tp) res_tp res_eff in
      let (env, rpat, _) =
        Pattern.check_arg_scheme env arg (T.Scheme.of_type r_tp) in
      let (body, _) = check_expr_type env body res_tp res_eff in
      let (x, body) = ExprUtils.arg_match rpat body res_tp (Some res_eff) in
      let e = make (T.EEffect(make (T.EVar lx), x, body, tp)) in
      (e, Impure)

    | None ->
      Error.fatal (Error.unbound_the_label ~pos)
    end

  | EExtern name ->
    (make (T.EExtern(name, tp)), Pure)

  | EAnnot(e', tp') ->
    let tp' = Type.tr_ttype env tp' in
    Error.check_unify_result ~pos
      (Unification.subtype env tp' tp)
      ~on_error:(Error.expr_type_mismatch ~env tp' tp);
    check_expr_type env e' tp' eff

  | ERepl def_seq ->
    (check_repl_def_seq ~tcfix env ImplicitEnv.empty def_seq tp eff, Impure)
