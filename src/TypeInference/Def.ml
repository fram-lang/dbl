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

let add_label_effect env eff_opt =
  match eff_opt with
  | None     -> Env.add_the_effect env
  | Some arg -> Type.check_type_arg env arg T.Kind.k_effect

(* ========================================================================= *)
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
    let (ims2, body) = ExprUtils.inst_args_match ims2 body tp None in
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
    let meff = match_effect (ret_effect_join r_eff2 r_eff3) eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let res_tp = bidir_result req resp in
    (make e2 (T.EMatch(e1, [(pat, e2)], res_tp, meff)), resp,
      ret_effect_joins [ r_eff1; r_eff2; r_eff3 ])

  | DMethodFn(public, x, method_name) ->
    let env = Env.add_method_fn ~public env x method_name in
    cont.run env ienv req eff

  | DLabel(eff_opt, pat) ->
    let scope = Env.scope env in
    let tp0  = Env.fresh_uvar env T.Kind.k_type in
    let eff0 = Env.fresh_uvar env T.Kind.k_effrow in
    let (env, l_eff) = add_label_effect env eff_opt in
    let l_tp = T.Type.t_label (T.Type.t_var l_eff) tp0 eff0 in
    let (env, pat, names, _) =
      Pattern.check_type ~env ~scope:(Env.scope env) pat l_tp in
    let ienv = ImplicitEnv.shadow_names ienv names in
    let (e2, resp, _) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let res_tp = bidir_result req resp in
    let x = Var.fresh ~name:"lbl" () in
    let dd =
      T.DD_Label { tvar = l_eff; var = x; delim_tp = tp0; delim_eff = eff0 }
    in
    (make e2 (T.EData([dd],
      (make e2 (T.EMatch({ def with data = T.EVar x},
        [(pat, e2)], res_tp, Some eff))))),
      resp, Impure)

  | DHandlePat(eff_opt, pat, e1) ->
    let env0 = env in
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let (e1, tp_h, r_eff1) = infer_expr_type env1 e1 eff in
    ImplicitEnv.end_generalize_impure ~env:env1 ~pos:e1.pos ims tp_h;
    begin match Unification.to_handler env tp_h with
    | H_Handler(a, cap_tp, tp_in, eff_in, tp_out, eff_out) ->
      (* extend the environment *)
      let (env, eff_tvar) = Env.add_the_effect ~pos env in
      let env =
        Type.check_type_alias_binder_opt env eff_opt (T.Type.t_var eff_tvar) in
      let sub = T.Subst.rename_to_fresh T.Subst.empty a eff_tvar in
      let cap_tp = T.Type.subst sub cap_tp in
      let tp_in  = T.Type.subst sub tp_in in
      let eff_in = T.Effect.cons eff_tvar (T.Type.subst sub eff_in) in
      let (env, pat, names, _) =
        Pattern.check_type ~env ~scope:(Env.scope env) pat cap_tp in
      let ienv = ImplicitEnv.shadow_names ienv names in
      (* check result's type and effect *)
      let resp : (_, dir) response =
        match req with
        | Infer    -> Infered tp_out
        | Check tp ->
          Error.check_unify_result ~pos
            (Unification.subtype env0 tp_out tp)
            ~on_error:(Error.expr_type_mismatch ~env:env0 tp_out tp);
          Checked
      in
      Error.check_unify_result ~pos
        (Unification.subeffect env0 eff_out eff)
        ~on_error:(Error.expr_effect_mismatch ~env:env0 eff_out eff);
      (* check the body *)
      let (e2, Checked, _) = cont.run env ienv (Check tp_in) eff_in in
      let cap_x = Var.fresh ~name:"cap" () in
      (make e2 (T.EHandle(eff_tvar, cap_x, cap_tp, e1,
        (make e2 (T.EMatch({def with data = T.EVar cap_x},
          [(pat, e2)], tp_in, Some eff_in))))),
        resp, Impure)
    | H_No ->
      Error.fatal (Error.expr_not_handler ~pos:e1.pos ~env tp_h)
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

  | DData(public, name, args, ctors) ->
    let scope = Env.scope env in
    let (data_env, args) = Type.tr_named_type_args env args in
    let kind  = DataType.kind args in
    let ctors = DataType.check_ctor_decls ~data_targs:args data_env ctors in
    let (env, x) = Env.add_tvar ~pos env ~public name kind in
    let (env, dd) =
      DataType.finalize_check ~nonrec_scope:scope env x ~name args ctors in
    let (e, resp, r_eff) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EData([dd], e)), resp, r_eff)

  | DRec defs ->
    let scope = Env.scope env in
    let (env, dds, fds, r_eff1) =
      RecDefs.check_rec_defs ~tcfix env ienv defs in
    let (e2, resp, r_eff2) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let r_eff = ret_effect_join r_eff1 r_eff2 in
    (make e2 (T.EData(dds, make e2 (T.ELetRec(fds, e2)))), resp, r_eff)

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
