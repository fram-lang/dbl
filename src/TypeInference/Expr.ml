(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Type of continuation used to type-checking of definitions. It is defined
  as record, in order to allow it to be polymorphic in a direction of type
  checking. *)
type def_cont =
  { run : 'dir.
      Env.t -> ImplicitEnv.t -> (T.typ, 'dir) request -> T.effect ->
        T.expr * (T.typ, 'dir) response * ret_effect
  }

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
(** Infer scheme of type variable *)
let infer_var_scheme ~pos env x =
  match Env.lookup_var env x with
  | Some (x, sch) ->
    ({ T.pos = pos; T.data = T.EVar x }, sch)
  | None ->
    Error.fatal (Error.unbound_var ~pos x)

(* ------------------------------------------------------------------------- *)
(** Infer scheme of a named implicit *)
let infer_implicit_scheme ~pos env name =
  match Env.lookup_implicit env name with
  | Some (x, sch, on_use) ->
    on_use pos;
    ({ T.pos = pos; T.data = T.EVar x }, sch)
  | None ->
    Error.fatal (Error.unbound_implicit ~pos name)

(* ------------------------------------------------------------------------- *)
(** Infer scheme of a constructor of ADT *)
let infer_ctor_scheme ~pos env c =
  match Env.lookup_ctor env c with
  | Some (idx, info) ->
    let ctor = List.nth info.adt_ctors idx in
    let targs = info.adt_args @ ctor.ctor_targs in
    let sch = {
        T.sch_targs = targs;
        T.sch_named = ctor.ctor_named;
        T.sch_body  = T.Type.t_pure_arrows ctor.ctor_arg_schemes info.adt_type
      } in
    (ExprUtils.ctor_func ~pos idx info, sch)
  | None ->
    Error.fatal (Error.unbound_constructor ~pos c)

(* ------------------------------------------------------------------------- *)
(** Infer scheme of a polymorphic expression. The effect of en expression is
  always in the check-mode. It returns a tuple, that contains the context of
  the polymorphic expression (computing polymorphic expression may have some
  effects, that should be performed before explicit instantiation), the
  translated polymorphic expression, its scheme, and the effect. *)
let infer_poly_scheme env (e : S.poly_expr) eff =
  let pos = e.pos in
  match e.data with
  | EVar  x ->
    let (e, sch) = infer_var_scheme ~pos env x in
    (Fun.id, e, sch, Pure)
  | EImplicit n ->
    let (e, sch) = infer_implicit_scheme ~pos env n in
    (Fun.id, e, sch, Pure)
  | ECtor c ->
    let (e, sch) = infer_ctor_scheme ~pos env c in
    (Fun.id, e, sch, Pure)

(* ------------------------------------------------------------------------- *)
(** Infer type of an expression. The effect of an expression is always in
  the check mode. However, pure expressions may returns an information that
  they are pure (see [ret_effect] type). *)
let rec infer_expr_type env (e : S.expr) eff =
  let make data = { e with data = data } in
  match e.data with
  | EMatch _ | EEffect _ | ERepl _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, tp, r_eff)

  | EUnit ->
    (make T.EUnit, T.Type.t_unit, Pure)

  | EPoly(e, tinst, inst) ->
    let (p_ctx, e, sch, r_eff1) = infer_poly_scheme env e eff in
    Uniqueness.check_type_inst_uniqueness tinst;
    Uniqueness.check_inst_uniqueness inst;
    let tinst = Type.check_type_insts env tinst sch.sch_targs in
    let (sub, tps) = ExprUtils.guess_types ~tinst env sch.sch_targs in
    let e = ExprUtils.make_tapp e tps in
    let named = List.map (T.NamedScheme.subst sub) sch.sch_named in
    let (i_ctx, inst, r_eff2) = check_explicit_insts env named inst eff in
    let e = ExprUtils.instantiate_named_params env e named inst in
    let tp = T.Type.subst sub sch.sch_body in
    (p_ctx (i_ctx e), tp, ret_effect_join r_eff1 r_eff2)

  | EFn(arg, body) ->
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    let body_eff = Env.fresh_uvar env T.Kind.k_effect in
    let (env, pat, sch, r_eff1) = Pattern.infer_arg_scheme env arg in
    let (body, r_eff2) = check_expr_type env body tp2 body_eff in
    let (x, body) = ExprUtils.arg_match pat body tp2 body_eff in
    begin match ret_effect_join r_eff1 r_eff2 with
    | Pure ->
      let b = Unification.subeffect env body_eff T.Effect.pure in
      assert b;
      (make (T.EPureFn(x, sch, body)), T.Type.t_pure_arrow sch tp2, Pure)
    | Impure ->
      (make (T.EFn(x, sch, body)), T.Type.t_arrow sch tp2 body_eff, Pure)
    end

  | EApp(e1, e2) ->
    let (e1, ftp, r_eff1) = infer_expr_type env e1 eff in
    begin match Unification.to_arrow env ftp with
    | Arr_UVar -> assert false
    | Arr_Pure(sch, vtp) ->
      let (e2, r_eff2) = check_actual_arg env e2 sch eff in
      (make (T.EApp(e1, e2)), vtp, ret_effect_join r_eff1 r_eff2)
    | Arr_Impure(sch, vtp, f_eff) ->
      let (e2, r_eff2) = check_actual_arg env e2 sch eff in
      if not (Unification.subeffect env f_eff eff) then
        Error.report (Error.func_effect_mismatch ~pos:e1.pos ~env f_eff eff);
      (make (T.EApp(e1, e2)), vtp, Impure)
    | Arr_No ->
      Error.fatal (Error.expr_not_function ~pos:e1.pos ~env ftp)
    end

  | EDefs(defs, e) ->
    let (e, Infered tp, r_eff) =
      check_defs env ImplicitEnv.empty defs Infer eff
        { run = fun env _ req eff -> tr_expr env e req eff }
    in
    (e, tp, r_eff)

  | EHandler h ->
    let (env, a) = Env.add_the_effect env in
    let res_tp  = Env.fresh_uvar env T.Kind.k_type in
    let res_eff = Env.fresh_uvar env T.Kind.k_effect in
    let (env, lx) =
      Env.add_the_label env (T.Effect.singleton a) res_tp res_eff in
    let (h, tp, r_eff) = infer_expr_type env h T.Effect.pure in
    begin match r_eff with
    | Pure -> ()
    | Impure -> Error.report (Error.impure_handler ~pos:e.pos)
    end;
    let e = make (T.EHandler(a, lx, res_tp, res_eff, h)) in
    (e, T.Type.t_handler a tp res_tp res_eff, Pure)

(* ------------------------------------------------------------------------- *)
(** Check type and effect of an expression. Returns also information about
  the purity of an expression. *)
and check_expr_type env (e : S.expr) tp eff =
  let make data = { e with data = data } in
  match e.data with
  | EUnit | EPoly _ | EApp _ ->
    check_expr_type_default env e tp eff

  | EFn(arg, body) ->
    begin match Unification.from_arrow env tp with
    | Arr_UVar ->
      check_expr_type_default env e tp eff

    | Arr_Pure(sch, tp2) ->
      let (env, pat, r_eff1) = Pattern.check_arg_scheme env arg sch in
      let (body, r_eff2) = check_expr_type env body tp2 T.Effect.pure in
      if ret_effect_join r_eff1 r_eff2 <> Pure then
        Error.report (Error.func_not_pure ~pos:e.pos);
      let (x, body) = ExprUtils.arg_match pat body tp2 T.Effect.pure in
      (make (T.EPureFn(x, sch, body)), Pure)

    | Arr_Impure(sch, tp2, eff) ->
      let (env, pat, _) = Pattern.check_arg_scheme env arg sch in
      let (body, _) = check_expr_type env body tp2 eff in
      let (x, body) = ExprUtils.arg_match pat body tp2 eff in
      (make (T.EFn(x, sch, body)), Pure)

    | Arr_No ->
      Error.report (Error.expr_not_function_ctx ~pos:e.pos ~env tp);
      let (e, _, r_eff) = infer_expr_type env e eff in
      (e, r_eff)
    end

  | EDefs(defs, e) ->
    let (e, Checked, r_eff) =
      check_defs env ImplicitEnv.empty defs (Check tp) eff
        { run = fun env _ req eff -> tr_expr env e req eff }
    in
    (e, r_eff)

  | EMatch(me, []) ->
    let (me, me_tp, _) = infer_expr_type env me eff in
    begin match T.Type.whnf me_tp with
    | Whnf_Neutral(NH_Var x, targs_rev) ->
      begin match Env.lookup_adt env x with
      | Some { adt_ctors = []; adt_proof; _ } ->
        let targs = List.rev targs_rev in
        let proof = ExprUtils.make_tapp adt_proof targs in
        (make (T.EMatchEmpty(proof, me, tp, eff)), Impure)

      | Some { adt_ctors = _ :: _; _ } ->
        Error.fatal (Error.empty_match_on_nonempty_adt ~pos:e.pos ~env me_tp)
      | None ->
        Error.fatal (Error.empty_match_on_non_adt ~pos:e.pos ~env me_tp)
      end

    | Whnf_Unit | Whnf_Neutral(NH_UVar _, _) | Whnf_PureArrow _
    | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
      Error.fatal (Error.empty_match_on_non_adt ~pos:e.pos ~env me_tp)

    | Whnf_Effect _ ->
      failwith "Internal kind error"
    end

  | EMatch(e, cls) ->
    let (e, e_tp, _) = infer_expr_type env e eff in
    let (cls, r_eff) = check_match_clauses env e_tp cls tp eff in
    (make (T.EMatch(e, cls, tp, eff)), r_eff)

  | EHandler h ->
    begin match Unification.from_handler env tp with
    | H_Handler(b, tp, tp0, eff0) ->
      let (env, a) = Env.add_the_effect env in
      let sub  = T.Subst.rename_to_fresh T.Subst.empty b a in
      let tp   = T.Type.subst sub tp in
      let tp0  = T.Type.subst sub tp0 in
      let eff0 = T.Type.subst sub eff0 in
      let (env, l) = Env.add_the_label env (T.Effect.singleton a) tp0 eff0 in
      let (h, r_eff) = check_expr_type env h tp T.Effect.pure in
      begin match r_eff with
      | Pure -> ()
      | Impure -> Error.report (Error.impure_handler ~pos:e.pos)
      end;
      let e = make (T.EHandler(a, l, tp0, eff0, h)) in
      (e, Pure)

    | H_No ->
      Error.fatal (Error.expr_not_handler_ctx ~pos:e.pos ~env tp)
    end

  | EEffect(arg, body) ->
    begin match Env.lookup_the_label env with
    | Some(lx, l_eff, res_tp, res_eff) ->
      if not (Unification.subeffect env l_eff eff) then
        Error.report (Error.expr_effect_mismatch ~pos:e.pos ~env l_eff eff);
      let r_tp = T.Type.t_arrow (T.Scheme.of_type tp) res_tp res_eff in
      let (env, rpat, _) =
        Pattern.check_arg_scheme env arg (T.Scheme.of_type r_tp) in
      let (body, _) = check_expr_type env body res_tp res_eff in
      let (x, body) = ExprUtils.arg_match rpat body res_tp res_eff in
      let e = make (T.EEffect(make (T.EVar lx), x, body, tp)) in
      (e, Impure)

    | None ->
      Error.fatal (Error.unbound_the_label ~pos:e.pos)
    end

  | ERepl def_seq ->
    (check_repl_def_seq env ImplicitEnv.empty def_seq tp eff, Impure)

(** Default action in type-check mode: switching to infer mode *)
and check_expr_type_default env (e : S.expr) tp eff =
  let pos = e.pos in
  let (e, tp', r_eff) = infer_expr_type env e eff in
  if not (Unification.subtype env tp' tp) then
    Error.report (Error.expr_type_mismatch ~pos ~env tp' tp);
  (e, r_eff)

(** Bidirectional type-checker of expressions *)
and tr_expr :
  type dir. Env.t -> S.expr -> (T.typ, dir) request -> T.effect ->
    (T.expr * (T.typ, dir) response * ret_effect) =
  fun env e tp_req eff ->
  match tp_req with
  | Infer ->
    let (e, tp, r_eff) = infer_expr_type env e eff in
    (e, Infered tp, r_eff)
  | Check tp ->
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, Checked, r_eff)

(* ------------------------------------------------------------------------- *)
(** Check the scheme of an actual parameter of a function *)
and check_actual_arg env (arg : S.expr) sch eff =
  let (env, tvars, named, body_tp) = Env.open_scheme env sch in
  let (body, res_eff) =
    match tvars, named with
    | [], [] -> check_expr_type env arg body_tp eff
    | _ ->
      let (body, res_eff) = check_expr_type env arg body_tp T.Effect.pure in
      begin match res_eff with
      | Pure -> ()
      | Impure -> Error.report (Error.func_not_pure ~pos:arg.pos) 
      end;
      (body, Pure)
  in
  (ExprUtils.make_tfun tvars (ExprUtils.make_nfun named body), res_eff)

(* ------------------------------------------------------------------------- *)
(** Check explicit instantiations against given list of named parameters (from
  type scheme). It returns context (represented as meta-function) that
  preserves the order of computations, list of checked instantiations, and
  the effect. *)
and check_explicit_insts env named insts eff =
  match insts with
  | [] -> (Fun.id, [], Pure)
  | { data = (n, e); pos } :: insts ->
    let make data = { T.data; T.pos } in
    let n = Name.tr_name env n in
    begin match T.Name.assoc n named with
    | Some sch ->
      let (e, r_eff1) = check_actual_arg env e sch eff in
      let (ctx, insts, r_eff2) = check_explicit_insts env named insts eff in
      let x = Var.fresh () in
      let ctx e0 = make (T.ELet(x, sch, e, ctx e0)) in
      let insts = (n, make (T.EVar x)) :: insts in
      (ctx, insts, ret_effect_join r_eff1 r_eff2)
    | None ->
      Error.warn (Error.redundant_named_parameter ~pos n);
      let (e, tp, r_eff1) = infer_expr_type env e eff in
      let sch = T.Scheme.of_type tp in
      let (ctx, insts, r_eff2) = check_explicit_insts env named insts eff in
      let x = Var.fresh () in
      let ctx e0 = make (T.ELet(x, sch, e, ctx e0)) in
      (ctx, insts, ret_effect_join r_eff1 r_eff2)
    end

(* ------------------------------------------------------------------------- *)
(** Check type and effect of a block of definitions. It uses bidirectional
  type checking, and pass the extended environment to the body-generating
  continuation. *)
and check_defs : type dir.
  Env.t -> ImplicitEnv.t -> S.def list ->
    (T.typ, dir) request -> T.effect -> def_cont ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun env ienv defs req eff cont ->
  match defs with
  | [] -> cont.run env ienv req eff
  | def :: defs ->
    check_def env ienv def req eff
      { run = fun env ienv req eff -> check_defs env ienv defs req eff cont }

and check_def : type dir.
  Env.t -> ImplicitEnv.t -> S.def ->
    (T.typ, dir) request -> T.effect -> def_cont ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun env ienv def req eff cont ->
  let make (e : T.expr) data =
    { T.pos  = Position.join def.pos e.pos;
      T.data = data
    } in
  match def.data with
  | DLetId(id, e1) ->
    let (sch, e1, r_eff1) = check_let env ienv e1 eff in
    let (env, ienv, x) = ImplicitEnv.add_poly_id env ienv id sch in
    let (e2, resp, r_eff2) = cont.run env ienv req eff in
    (make e2 (T.ELet(x, sch, e1, e2)), resp, ret_effect_join r_eff1 r_eff2)

  | DLetFun(id, targs, nargs, body) ->
    let (body_env, tvars) = Type.tr_named_type_args env targs in 
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
    let ims1 = ImplicitEnv.end_generalize_pure ims1 in
    let (body, sch) = ExprUtils.generalize env tvars (ims1 @ ims2) body tp in
    let (env, ienv, x) = ImplicitEnv.add_poly_id env ienv id sch in
    let (e2, resp, r_eff) = cont.run env ienv req eff in
    (make e2 (T.ELet(x, sch, body, e2)), resp, r_eff)

  | DLetPat(pat, e1) ->
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let scope = Env.scope env1 in
    let (e1, tp, r_eff1) = infer_expr_type env1 e1 eff in
    ImplicitEnv.end_generalize_impure ims;
    let (env, pat, names, r_eff2) = Pattern.check_type ~env ~scope pat tp in
    let ienv = ImplicitEnv.shadow_names ienv names in
    let (e2, resp, r_eff3) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let res_tp = bidir_result req resp in
    (make e2 (T.EMatch(e1, [(pat, e2)], res_tp, eff)), resp,
      ret_effect_joins [ r_eff1; r_eff2; r_eff3 ])

  | DHandlePat(pat, eh) ->
    let (env_h, ims) = ImplicitEnv.begin_generalize env ienv in
    let (eh, eh_tp, _) = infer_expr_type env_h eh eff in
    (* TODO: effect capability may have a scheme instead of type *)
    ImplicitEnv.end_generalize_impure ims;
    begin match Unification.to_handler env eh_tp with
    | H_Handler(a, cap_tp, res_tp, res_eff) ->
      let (env1, h_eff) = Env.add_anon_tvar env T.Kind.k_cleffect in
      let sub = T.Subst.rename_to_fresh T.Subst.empty a h_eff in
      let cap_tp  = T.Type.subst sub cap_tp  in
      let res_tp  = T.Type.subst sub res_tp  in
      let res_eff = T.Type.subst sub res_eff in
      let (env1, pat, names, _) =
        Pattern.check_type ~env:env1 ~scope:(Env.scope env1) pat cap_tp in
      let ienv = ImplicitEnv.shadow_names ienv names in
      let body_eff = T.Effect.cons h_eff res_eff in
      let (e, Checked, _) = cont.run env1 ienv (Check res_tp) body_eff in
      let (x, e) = ExprUtils.arg_match pat e res_tp body_eff in
      let pos = Position.join def.pos e.pos in
      if not (Unification.subeffect env res_eff eff) then
        Error.report (Error.expr_effect_mismatch ~pos ~env res_eff eff);
      let resp : (_, dir) response =
        match req with
        | Infer    -> Infered (res_tp)
        | Check tp ->
          if not (Unification.subtype env res_tp tp) then
            Error.report (Error.expr_type_mismatch ~pos ~env res_tp tp);
          Checked
      in
      (make e (T.EHandle(h_eff, x, e, eh, res_tp, res_eff)), resp, Impure)

    | H_No ->
      Error.fatal (Error.expr_not_handler ~pos:eh.pos ~env eh_tp)
    end

  | DImplicit n ->
    let ienv = ImplicitEnv.declare_implicit ienv n in
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

  | DReplExpr e1 ->
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let scope = Env.scope env1 in
    let (e1, tp1, r_eff1) = check_repl_expr env e1 eff in
    ImplicitEnv.end_generalize_impure ims;
    let (e, resp, r_eff2) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EReplExpr(e1, tp1, e)), resp, ret_effect_join r_eff1 r_eff2)

(* ------------------------------------------------------------------------- *)
(** Check let-definition *)
and check_let env ienv body eff =
  let (body_env, ims) = ImplicitEnv.begin_generalize env ienv in
  let (body, tp, r_eff) = infer_expr_type body_env body eff in
  (* Purity restriction: check if r_eff is pure. If so, then generalize type
    of an expression *)
  match r_eff with
  | Pure ->
    let ims = ImplicitEnv.end_generalize_pure ims in
    let (body, sch) = ExprUtils.generalize env [] ims body tp in
    (sch, body, Pure)
  | Impure ->
    ImplicitEnv.end_generalize_impure ims;
    let sch = T.Scheme.of_type tp in
    (sch, body, Impure)

(* ------------------------------------------------------------------------- *)
(** Check a pattern-matching clause
  In [check_match_clause env tp cl res_tp res_eff] the parameters have the
  following meaning:
  - [env]     -- an environment
  - [tp]      -- type of the matched expression
  - [cl]      -- clause
  - [res_tp]  -- returned type
  - [res_eff] -- returned effect *)
and check_match_clause env tp (cl : S.match_clause) res_tp res_eff =
  match cl.data with
  | Clause(pat, body) ->
    let scope = Env.scope env in
    let (env, pat, _, r_eff1) = Pattern.check_type ~env ~scope pat tp in
    let (body, r_eff2) = check_expr_type env body res_tp res_eff in
    (pat, body, ret_effect_join r_eff1 r_eff2)

and check_match_clauses env tp cls res_tp res_eff =
  let (r_eff, cls) = List.fold_left_map
    (fun r_eff1 cl ->
      let (pat, body, r_eff2) = check_match_clause env tp cl res_tp res_eff in
      (ret_effect_join r_eff1 r_eff2, (pat, body)))
    Pure
    cls
  in
  (cls, r_eff)

(* ------------------------------------------------------------------------- *)
(** Check the sequence of REPL definitions, provided by a user. Always
  in type-check mode. *)
and check_repl_def_seq env ienv def_seq tp eff =
  let func () =
    match def_seq () with
    | Seq.Nil -> assert false
    | Seq.Cons(def, def_seq) ->
      let cont (type dir) env ienv (tp_req : (_, dir) request) eff :
          _ * (_, dir) response * _ =
        match tp_req with
        | Check tp ->
          let e = check_repl_def_seq env ienv def_seq tp eff in
          (e, Checked, Impure)
        | Infer ->
          let tp = Env.fresh_uvar env T.Kind.k_type in
          let e = check_repl_def_seq env ienv def_seq tp eff in
          (e, Infered tp, Impure)
      in
      let (e, Checked, _) =
        check_def env ienv def (Check tp) eff { run = cont } in
      e
  in
  let e =
    { T.pos  = Position.nowhere;
      T.data = T.ERepl(func, tp, eff)
    } in
  e

(* ------------------------------------------------------------------------- *)
(** Check expression put into REPL *)
and check_repl_expr env e eff =
  let (e, tp, r_eff) = infer_expr_type env e eff in
  (e, "?", r_eff)
