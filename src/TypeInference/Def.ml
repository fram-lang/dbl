(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for definitions *)

open Common
open TypeCheckFix

(** Information about the label used by a handler *)
type label_info =
  { l_expr : T.expr;
      (** Expression that evaluates to the label *)

    l_ctx : T.expr -> T.expr;
      (** Context that generates fresh label when necessary *)

    l_eff : T.effect;
      (** Effect provided by this label *)

    l_sub : T.tvar -> T.subst;
      (** Substitution generating function, used to instantiate capability *)

    l_delim_tp : T.typ;
      (** Type of the delimiter *)

    l_delim_eff : T.effrow
      (** Effect of the delimiter *)
  }

(** Check if given expression is a monomorphic variable *)
let is_monomorphic_var env (e : S.expr) =
  match e.data with
  | EPoly(p, _, _) ->
    let sch_opt =
      match p.data with
      | EVar x ->
        begin match Env.lookup_var env x with
        | None | Some (VI_Ctor _) | Some (VI_MethodFn _) -> None
        | Some (VI_Var(_, sch)) -> Some sch
        end
      | EImplicit name ->
        Option.map (fun (_, sch, _) -> sch) (Env.lookup_implicit env name)
      | EMethod _ -> None
    in
    begin match sch_opt with
    | Some { sch_targs = []; sch_named = []; sch_body = _ } -> true
    | _ -> false
    end
  | _ -> false

(* ------------------------------------------------------------------------- *)
(** Shrink scope of type response. On error, raise escaping scope error
  for given position *)
let type_resp_in_scope (type dir) ~env ~pos ~scope
    (resp : (T.typ, dir) response) : (T.typ, dir) response =
  match resp with
  | Infered tp ->
    (* TODO: tp can be raised to some supertype in the scope *)
    begin match T.Type.try_shrink_scope ~scope tp with
    | Ok   () -> Infered tp
    | Error x ->
      Error.fatal (Error.type_escapes_its_scope ~pos ~env x)
    end
  | Checked -> Checked

(* ------------------------------------------------------------------------- *)
(** Check let-definition *)
let check_let ~tcfix ~pos env ienv body eff =
  let open (val tcfix : TCFix) in
  let (body_env, ims) = ImplicitEnv.begin_generalize env ienv in
  let (body, tp, r_eff) = infer_expr_type body_env body eff in
  (* Purity restriction: check if r_eff is pure. If so, then generalize type
    of an expression *)
  match r_eff with
  | Pure ->
    let (targs, ims) = ImplicitEnv.end_generalize_pure ims (T.Type.uvars tp) in
    (* TODO: make sure that names do not overlap *)
    let (body, sch) = ExprUtils.generalize ~pos targs ims body tp in
    (sch, body, Pure)
  | Impure ->
    ImplicitEnv.end_generalize_impure ~env:body_env ~pos:body.pos ims tp;
    let sch = T.Scheme.of_type tp in
    (sch, body, Impure)

(* ------------------------------------------------------------------------- *)
(** First pass of checking recursive function: guessing its scheme *)
let prepare_rec_fun (env, ienv) (fd : S.rec_fun) =
  let (RecFun(name, targs, nargs, body)) = fd.data in
  let (body_env, tvars) = Type.tr_named_type_args env targs in
  let (body_env, named, r_eff1) =
    Pattern.infer_named_arg_schemes body_env nargs in
  (* TODO: use type annotation to obtain this type *)
  let body_tp = Env.fresh_uvar body_env T.Kind.k_type in
  let sch =
    { T.sch_targs = tvars;
      T.sch_named = List.map (fun (name, _, sch) -> (name, sch)) named;
      T.sch_body  = body_tp
    } in
  let (env, ienv, x) = ImplicitEnv.add_poly_id ~pos:fd.pos env ienv name sch in
  let fd = (fd.pos, name, x, sch, targs, nargs, body) in
  ((env, ienv), fd)

(** Second pass of checking recursive function: type-checking *)
let check_rec_fun ~tcfix env (pos, name, x, sch, targs, nargs, body) =
  let open (val tcfix : TCFix) in
  let (body_env, tvars) = Type.tr_named_type_args env targs in
  let (body_env, named, r_eff1) =
    Pattern.infer_named_arg_schemes body_env nargs in
  let body_tp = Env.fresh_uvar body_env T.Kind.k_type in
  let sch' =
    { T.sch_targs = tvars;
      T.sch_named = List.map (fun (name, _, sch) -> (name, sch)) named;
      T.sch_body  = body_tp
    } in
  if Unification.subscheme env sch' sch <> Unify_Success then
    (* Both schemes come from the same definition. They should be unifiable *)
    assert false;
  let (body, r_eff2) = check_expr_type body_env body body_tp T.Effect.pure in
  begin match ret_effect_join r_eff1 r_eff2 with
  | Pure -> ()
  | Impure -> Error.report (Error.func_not_pure ~pos)
  end;
  let (named, body) =
    ExprUtils.inst_args_match named body body_tp T.Effect.pure in
  let body = ExprUtils.make_nfun named body in
  (pos, name, x, sch, tvars, body)

(** Third pass of checking recursive function: collecting unification
  variables *)
let collect_rec_fun_uvars uvs (_, _, _, sch, _, _) =
  T.Scheme.collect_uvars sch uvs

(** Fourth pass of checking recursive function: building final environment *)
let add_rec_fun tvs1 ims (env, ienv) (pos, name, x, sch, tvs2, body) =
  let make data = { T.pos = pos; T.data = data } in
  let make_app ims f =
    List.fold_left
      (fun f (_, x, _) -> make (T.EApp(f, make (T.EVar x))))
      f ims in
  let make_tapp tvs f =
    List.fold_left
      (fun f (_, x) -> make (T.ETApp(f, T.Type.t_var x)))
      f tvs in
  let y_sch =
    { T.sch_targs = tvs1 @ sch.T.sch_targs;
      T.sch_named =
        List.map (fun (name, _, sch) -> (name, sch)) ims @ sch.T.sch_named;
      T.sch_body  = sch.T.sch_body
    } in
  let (env, ienv, y) = ImplicitEnv.add_poly_id ~pos env ienv name y_sch in
  (* Less-generalized form of this function *)
  let x_body =
    ExprUtils.make_tfun tvs2
      (make (T.EVar y) |> make_tapp tvs1 |> make_tapp tvs2 |> make_app ims) in
  let fd = (pos, y, y_sch, x, sch, x_body, tvs1 @ tvs2, ims, body) in
  ((env, ienv), fd)

(** Fifth and final pass of checking recursive function: build an expression
  with local definitions of less-generalized functions *)
let finalize_rec_fun fds (pos, y, y_sch, _, _, _, tvs, ims, body) =
  let make data = { T.pos = pos; T.data = data } in
  let build_local_inst (_, _, _, x, x_sch, x_body, _, _, _) body =
    make (T.ELet(x, x_sch, x_body, body)) in
  let build_local_ctx body =
    List.fold_right build_local_inst fds body in
  let rec update_body (body : T.expr) =
    let make data = { body with data = data } in
    match body.T.data with
    | EUnitPrf | ENum _ | EStr _ | EExtern _ -> body
    | EVar x ->
      if List.exists (fun (_, _, _, x', _, _, _, _, _) -> Var.equal x x') fds
      then
        Error.fatal (Error.non_productive_rec_def ~pos:body.pos);
      body
    | EFn(arg, arg_sch, body) ->
      make (T.EFn(arg, arg_sch, build_local_ctx body))
    | EPureFn(arg, arg_sch, body) ->
      make (T.EPureFn(arg, arg_sch, update_body body))
    | ETFun(x, body) ->
      make (T.ETFun(x, update_body body))
    | ECtor(prf, n, tps, args) ->
      make (T.ECtor(prf, n, tps, List.map update_body args))
    | EApp _ | ETApp _ | ELet _ | ELetRec _ | EData _ | EMatchEmpty _
    | EMatch _ | ELabel _ | EHandle _ | EHandler _ | EEffect _ | ERepl _
    | EReplExpr _ ->
      Error.fatal (Error.non_productive_rec_def ~pos:body.pos)
  in
  let body = update_body body in
  (y, y_sch, ExprUtils.make_tfun tvs (ExprUtils.make_nfun ims body))

(* ------------------------------------------------------------------------- *)
(** Check expression put into REPL *)
let check_repl_expr ~tcfix env ienv e eff =
  let open (val tcfix : TCFix) in
  let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
  let (e, tp, r_eff) = infer_expr_type env e eff in
  ImplicitEnv.end_generalize_impure ~pos:e.pos ~env:env1 ims tp;
  let pp_ctx = Pretty.empty_context () in
  (e, Pretty.type_to_string pp_ctx env tp, r_eff)

(* ------------------------------------------------------------------------- *)
let check_def : type dir. tcfix:tcfix ->
  Env.t -> ImplicitEnv.t -> S.def ->
    (T.typ, dir) request -> T.effrow -> def_cont ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun ~tcfix env ienv def req eff cont ->
  let open (val tcfix : TCFix) in
  let make (e : T.expr) data =
    { T.pos  = Position.join def.pos e.pos;
      T.data = data
    } in
  let pos = def.pos in
  match def.data with
  | DLetId(id, e1) ->
    let (sch, e1, r_eff1) = check_let ~tcfix ~pos env ienv e1 eff in
    let (env, ienv, x) =
      ImplicitEnv.add_poly_id ~pos env ienv id sch in
    let (e2, resp, r_eff2) = cont.run env ienv req eff in
    (make e2 (T.ELet(x, sch, e1, e2)), resp, ret_effect_join r_eff1 r_eff2)

  | DLetFun(id, targs, nargs, body) ->
    let (body_env, tvars2) = Type.tr_named_type_args env targs in 
    let (body_env, ims1) = ImplicitEnv.begin_generalize body_env ienv in
    let (body_env, ims2, r_eff1) =
      Pattern.infer_named_arg_schemes body_env nargs in
    let (body, tp, r_eff2) = infer_expr_type body_env body T.Effect.pure in
    begin match ret_effect_join r_eff1 r_eff2 with
    | Pure -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos:def.pos)
    end;
    (* TODO: check if [tp] is in proper scope (ims2 may bind some types) *)
    let (ims2, body) = ExprUtils.inst_args_match ims2 body tp T.Effect.pure in
    let (tvars1, ims1) =
      ImplicitEnv.end_generalize_pure ims1 (T.Type.uvars tp) in
    let (body, sch) =
      ExprUtils.generalize ~pos (tvars1 @ tvars2) (ims1 @ ims2) body tp in
    let (env, ienv, x) =
      ImplicitEnv.add_poly_id ~pos env ienv id sch in
    let (e2, resp, r_eff) = cont.run env ienv req eff in
    (make e2 (T.ELet(x, sch, body, e2)), resp, r_eff)

  | DLetPat(pat, e1) ->
    let scope = Env.scope env in
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let (e1, tp, r_eff1) = infer_expr_type env1 e1 eff in
    ImplicitEnv.end_generalize_impure ~env:env1 ~pos:e1.pos ims tp;
    let (env, pat, names, r_eff2) =
      Pattern.check_type ~env ~scope pat tp in
    let ienv = ImplicitEnv.shadow_names ienv names in
    let (e2, resp, r_eff3) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let res_tp = bidir_result req resp in
    (make e2 (T.EMatch(e1, [(pat, e2)], res_tp, eff)), resp,
      ret_effect_joins [ r_eff1; r_eff2; r_eff3 ])

  | DMethodFn(public, x, method_name) ->
    let env = Env.add_method_fn ~public env x method_name in
    cont.run env ienv req eff

  | DFunRec fds ->
    let (rec_env, ims) = ImplicitEnv.begin_generalize env ienv in
    let ((rec_env, _), fds) =
      List.fold_left_map prepare_rec_fun (rec_env, ienv) fds in
    let fds = List.map (check_rec_fun ~tcfix rec_env) fds in
    let uvars = List.fold_left collect_rec_fun_uvars T.UVar.Set.empty fds in
    let (tvars, ims) = ImplicitEnv.end_generalize_pure ims uvars in
    let ((env, ienv), fds) =
      List.fold_left_map (add_rec_fun tvars ims) (env, ienv) fds in
    let fds = List.map (finalize_rec_fun fds) fds in
    let (e2, resp, r_eff) = cont.run env ienv req eff in
    (make e2 (T.ELetRec(fds, e2)), resp, r_eff)

  | DLabel(eff_opt, pat) ->
    let scope = Env.scope env in
    let (env, l_eff) = Env.add_the_effect ~pos:def.pos env in
    let env =
      Type.check_type_alias_binder_opt env eff_opt (T.Type.t_var l_eff) in
    let tp0  = Env.fresh_uvar env T.Kind.k_type in
    let eff0 = Env.fresh_uvar env T.Kind.k_effrow in
    let l_tp = T.Type.t_label (T.Type.t_var l_eff) tp0 eff0 in
    let scope1 = Env.scope env in
    let (env, pat, names, _) =
      Pattern.check_type ~env ~scope:scope1 pat l_tp in
    let ienv = ImplicitEnv.shadow_names ienv names in
    let (e2, resp, _) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let res_tp = bidir_result req resp in
    let x = Var.fresh ~name:"lbl" () in
    (make e2 (T.ELabel(l_eff, x, tp0, eff0,
      (make e2 (T.EMatch({ def with data = T.EVar x},
        [(pat, e2)], res_tp, eff))))),
      resp, Impure)

  | DHandlePat
    { label; effect = eff_opt; cap_pat = pat; capability = eh;
      ret_clauses = rcs; fin_clauses = fcs } ->
    let env0 = env in
    let (lbl, env) =
      match label with
      | None ->
        let (env, l_eff) = Env.add_the_effect env in
        let env =
          Type.check_type_alias_binder_opt env eff_opt (T.Type.t_var l_eff) in
        let tp0  = Env.fresh_uvar env T.Kind.k_type in
        let eff0 = Env.fresh_uvar env T.Kind.k_effrow in
        let (env, x) = Env.add_the_label env (T.Type.t_var l_eff) tp0 eff0 in
        let ctx e =
          { T.pos  = Position.join def.pos e.T.pos;
            T.data = T.ELabel(l_eff, x, tp0, eff0, e)
          }
        in
        { l_expr      = { T.pos = def.pos; T.data = T.EVar x };
          l_ctx       = ctx;
          l_eff       = T.Type.t_var l_eff;
          l_sub       =
            (* It might be tempting to use [T.Subst.rename_to_fresh] here, but
              we cannot ensure that this freshly generated type variable do
              not appear in component of the handler type (and it is easy to
              find counter-example). However, for monomorphic handlers it
              could be done better. *)
            (fun a ->
              if is_monomorphic_var env eh then
                T.Subst.rename_to_fresh T.Subst.empty a l_eff
              else
                T.Subst.add_type T.Subst.empty a (T.Type.t_var l_eff));
          l_delim_tp  = tp0;
          l_delim_eff = eff0
        }, env
      | Some le ->
        let (env', ims) = ImplicitEnv.begin_generalize env ienv in
        let (le, le_tp, _) = infer_expr_type env' le eff in
        ImplicitEnv.end_generalize_impure ~env:env' ~pos:le.pos ims le_tp;
        begin match Unification.as_label env le_tp with
        | L_Label(l_eff, tp0, eff0) ->
          let env = Type.check_type_alias_binder_opt env eff_opt l_eff in
          let env = Env.add_the_effect_alias env l_eff in
          let (env, l_var) = Env.add_the_label env l_eff tp0 eff0 in
          let ctx e =
            { T.pos  = Position.join def.pos e.T.pos;
              T.data = T.ELet(l_var, T.Scheme.of_type le_tp, le, e)
            } in
          { l_expr      = { le with data = T.EVar l_var };
            l_ctx       = ctx;
            l_eff       = l_eff;
            l_sub       = (fun a -> T.Subst.add_type T.Subst.empty a l_eff);
            l_delim_tp  = tp0;
            l_delim_eff = eff0
          }, env

        | L_NoEffect ->
          Error.fatal (Error.unbound_the_effect ~pos:le.pos)

        | L_No ->
          Error.fatal (Error.expr_not_label ~pos:le.pos ~env le_tp)
        end
    in
    let env_f = env in
    let (env_h, ims) = ImplicitEnv.begin_generalize env ienv in
    let (eh, eh_tp, _) = infer_expr_type env_h eh eff in
    (* TODO: effect capability may have a scheme instead of type *)
    ImplicitEnv.end_generalize_impure ~env:env_h ~pos:eh.pos ims eh_tp;
    begin match Unification.to_handler env eh_tp with
    | H_Handler(a, cap_tp, res_tp, res_eff) ->
      let sub = lbl.l_sub a in
      let cap_tp  = T.Type.subst sub cap_tp  in
      let res_tp  = T.Type.subst sub res_tp  in
      let res_eff = T.Type.subst sub res_eff in
      Error.check_unify_result ~pos:def.pos
        (Unification.unify_type env lbl.l_delim_tp res_tp)
        ~on_error:(Error.delim_type_mismatch ~env lbl.l_delim_tp res_tp);
      Error.check_unify_result ~pos:def.pos
        (Unification.unify_type env lbl.l_delim_eff res_eff)
        ~on_error:(Error.delim_effect_mismatch ~env lbl.l_delim_eff res_eff);
      let (env, pat, names, _) =
        Pattern.check_type ~env ~scope:(Env.scope env) pat cap_tp in
      let ienv = ImplicitEnv.shadow_names ienv names in
      let (ret_x, body_tp, ret_body) =
        MatchClause.check_return_clauses ~tcfix env rcs res_tp res_eff in
      let body_eff = T.Effect.cons_eff lbl.l_eff res_eff in
      let (body, Checked, _) = cont.run env ienv (Check body_tp) body_eff in
      let (x, body) = ExprUtils.arg_match pat body body_tp body_eff in
      let pos = Position.join def.pos body.pos in
      Error.check_unify_result ~pos
        (Unification.subeffect env0 res_eff eff)
        ~on_error:(Error.expr_effect_mismatch ~env:env0 res_eff eff);
      let e =
        make body (T.EHandle
          { label      = lbl.l_expr;
            effect     = lbl.l_eff;
            cap_var    = x;
            body       = body;
            capability = eh;
            ret_var    = ret_x;
            ret_body   = ret_body;
            result_tp  = res_tp;
            result_eff = res_eff
          }) in
      let (e, tp, r_eff) =
        MatchClause.check_finally_clauses ~tcfix env_f fcs e res_tp req eff in
      (lbl.l_ctx e, tp, r_eff)

    | H_No ->
      Error.fatal (Error.expr_not_handler ~pos:eh.pos ~env eh_tp)
    end

  | DImplicit(n, args, sch) ->
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let (env1, args1) = Type.tr_named_type_args env1 args in
    let sch = Type.tr_scheme env1 sch in
    let (args2, ims) =
      ImplicitEnv.end_generalize_pure ims (T.Scheme.uvars sch) in
    assert (List.is_empty ims);
    let args = args1 @ args2 in
    let ienv = ImplicitEnv.declare_implicit ienv n args sch in
    cont.run env ienv req eff

  | DData dd ->
    let scope = Env.scope env in
    let (env, dd) = DataType.check_data_def env dd in
    let (e, resp, r_eff) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EData([dd], e)), resp, r_eff)

  | DDataRec dds ->
    let scope = Env.scope env in
    let (env, dds) = DataType.check_rec_data_defs env dds in
    let (e, resp, r_eff) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EData(dds, e)), resp, r_eff)

  | DModule(public, name, defs) ->
    let env = Env.enter_module env in
    check_defs env ImplicitEnv.empty defs req eff
      { run = fun env _ req eff ->
        let env = Env.leave_module env ~public name in
        cont.run env ienv req eff }

  | DOpen(public, path) ->
    begin match Env.lookup_module env path with
    | Some m ->
      let env = Env.open_module env ~public m in
      cont.run env ienv req eff
    | None ->
      Error.report (Error.unbound_module ~pos path);
      cont.run env ienv req eff
    end

  | DReplExpr e1 ->
    let scope = Env.scope env in
    let (e1, tp1, r_eff1) = check_repl_expr ~tcfix env ienv e1 eff in
    let (e, resp, r_eff2) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EReplExpr(e1, tp1, e)), resp, ret_effect_join r_eff1 r_eff2)

(* ------------------------------------------------------------------------- *)
let check_defs : type dir. tcfix:tcfix ->
  Env.t -> ImplicitEnv.t -> S.def list ->
    (T.typ, dir) request -> T.effrow -> def_cont ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun ~tcfix env ienv defs req eff cont ->
  let open (val tcfix : TCFix) in
  match defs with
  | [] -> cont.run env ienv req eff
  | def :: defs ->
    check_def env ienv def req eff
      { run = fun env ienv req eff -> check_defs env ienv defs req eff cont }
