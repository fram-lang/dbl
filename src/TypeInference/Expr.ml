(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

(* Author: Piotr Polesiuk, 2023,2024 *)

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
    let targs = info.adt_args @ ctor.ctor_tvars in
    let sch = {
        T.sch_tvars = targs;
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
  let pos = e.pos in
  let make data = { e with data = data } in
  match e.data with
  | EMatch _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, tp, r_eff)

  | EUnit ->
    (make T.EUnit, T.Type.t_unit, Pure)

  | EPoly(e, inst) ->
    let (p_ctx, e, sch, r_eff1) = infer_poly_scheme env e eff in
    Uniqueness.check_inst_uniqueness inst;
    let (sub, tps) = ExprUtils.guess_types env sch.sch_tvars in
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
    let scope = Env.scope env in
    let (env, e_gen, r_eff1) = check_defs env ImplicitEnv.empty defs eff in
    let (e, tp, r_eff2) = infer_expr_type env e eff in
    (* TODO: tp can be raised to some supertype in the scope *)
    begin match T.Type.try_shrink_scope ~scope tp with
    | Ok   () -> (e_gen e tp, tp, ret_effect_join r_eff1 r_eff2)
    | Error x ->
      Error.fatal (Error.type_escapes_its_scope ~pos ~env x)
    end

  | EHandle(pat, e1, h) ->
    (* Since type and effect of e1 is used both on covariant and contravariant
     position (return type and resumption respectively), we should guess
     them even in type-check mode. *)
    let res_tp  = Env.fresh_uvar env T.Kind.k_type   in
    let res_eff = Env.fresh_uvar env T.Kind.k_effect in
    let (env1, h_eff) = Env.add_anon_tvar env T.Kind.k_cleffect in
    (* TODO: effect capability may have a scheme instead of type *)
    let (h, x_tp) = infer_h_expr_type env h h_eff res_tp res_eff in
    let (env1, pat, _, _) =
      Pattern.check_type ~env:env1 ~scope:(Env.scope env1) pat x_tp in
    let e1_eff = T.Effect.cons h_eff res_eff in
    let (e1, _) = check_expr_type env1 e1 res_tp e1_eff in
    let (x, e1) = ExprUtils.arg_match pat e1 res_tp e1_eff in
    if not (Unification.subeffect env res_eff eff) then
      Error.report (Error.expr_effect_mismatch ~pos ~env res_eff eff);
    (make (T.EHandle(h_eff, x, e1, h, res_tp, res_eff)), res_tp, Impure)

  | ERepl func ->
    let tp = T.Type.t_unit in
    let e =
      make (T.ERepl(
        (fun () -> fst (check_expr_type env (func ()) tp eff)),
        eff))
    in (e, tp, Impure)

  | EReplExpr(e1, e2) ->
    let (e1, tp1, r_eff1) = check_repl_expr env e1 eff in
    let (e2, tp2, r_eff2) = infer_expr_type env e2 eff in
    (make (T.EReplExpr(e1, tp1, e2)), tp2, ret_effect_join r_eff1 r_eff2)

(* ------------------------------------------------------------------------- *)
(** Check type and effect of an expression. Returns also information about
  the purity of an expression. *)
and check_expr_type env (e : S.expr) tp eff =
  let make data = { e with data = data } in
  match e.data with
  | EUnit | EPoly _ | EApp _ | EHandle _ | ERepl _ ->
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
    let (env, e_gen, r_eff1) = check_defs env ImplicitEnv.empty defs eff in
    let (e, r_eff2) = check_expr_type env e tp eff in
    (e_gen e tp, ret_effect_join r_eff1 r_eff2)

  | EMatch(e, cls) ->
    let (e, e_tp, _) = infer_expr_type env e eff in
    let (cls, r_eff) = check_match_clauses env e_tp cls tp eff in
    (make (T.EMatch(e, cls, tp, eff)), r_eff)

  | EReplExpr(e1, e2) ->
    let (e1, tp1, r_eff1) = check_repl_expr env e1 eff in
    let (e2, r_eff2) = check_expr_type env e2 tp eff in
    (make (T.EReplExpr(e1, tp1, e2)), ret_effect_join r_eff1 r_eff2)

and check_expr_type_default env (e : S.expr) tp eff =
  let pos = e.pos in
  let (e, tp', r_eff) = infer_expr_type env e eff in
  if not (Unification.subtype env tp' tp) then
    Error.report (Error.expr_type_mismatch ~pos ~env tp' tp);
  (e, r_eff)

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
(** Check effect of a block of definitions. Returns extended environment, a
  function that extends expression with given list of definitions, and the
  effect of a definition block. *)
and check_defs env ienv (defs : S.def list) eff =
  match defs with
  | [] -> (env, (fun e _ -> e), Pure)
  | def :: defs ->
    let (env, ienv, e_gen1, r_eff1) = check_def env ienv def eff in
    let (env, e_gen2, r_eff2) = check_defs env ienv defs eff in
    (env, (fun e tp -> e_gen1 (e_gen2 e tp) tp), ret_effect_join r_eff1 r_eff2)

and check_def env ienv (def : S.def) eff =
  let make (e : T.expr) data =
    { T.pos  = Position.join def.pos e.pos;
      T.data = data
    } in
  match def.data with
  | DLetId(id, e1) ->
    let (sch, e1, r_eff) = check_let env ienv e1 eff in
    let (env, ienv, x) = ImplicitEnv.add_poly_id env ienv id sch in
    (env, ienv, (fun e _ -> make e (T.ELet(x, sch, e1, e))), r_eff)

  | DLetFun(id, targs, nargs, body) ->
    let (body_env, tvars) = Type.tr_type_args env targs in 
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
    (env, ienv, (fun e _ -> make e (T.ELet(x, sch, body, e))), Pure)

  | DLetPat(pat, e1) ->
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let scope = Env.scope env1 in
    let (e1, tp, r_eff1) = infer_expr_type env1 e1 eff in
    ImplicitEnv.end_generalize_impure ims;
    let (env, pat, names, r_eff2) = Pattern.check_type ~env ~scope pat tp in
    let ienv = ImplicitEnv.shadow_names ienv names in
    let ctx e res_tp = make e (T.EMatch(e1, [(pat, e)], res_tp, eff)) in
    (env, ienv, ctx, ret_effect_join r_eff1 r_eff2)

  | DImplicit n ->
    let ienv = ImplicitEnv.declare_implicit ienv n in
    (env, ienv, (fun e _ -> e), Pure)

  | DData dd ->
    let (env, dd) = DataType.check_data_def env dd in
    (env, ienv, (fun e _ -> make e (T.EData([dd], e))), Pure)

  | DDataRec dds ->
    let (env, dds) = DataType.check_rec_data_defs env dds in
    (env, ienv, (fun e _ -> make e (T.EData(dds, e))), Pure)

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
(** Infer type of an handler expression.
  In [infer_h_expr_type env h h_eff res_tp res_eff] the parameters have the
  following meaning:
  - [env]     -- an environment
  - [h]       -- handler expression
  - [h_eff]   -- a handled effect
  - [res_tp]  -- returned type
  - [res_eff] -- returned effect *)
and infer_h_expr_type env h h_eff res_tp res_eff =
  let make data = { h with data = data } in
  match h.data with
  | HEffect(x, r, body) ->
    let in_tp  = Env.fresh_uvar env T.Kind.k_type in
    let out_tp = Env.fresh_uvar env T.Kind.k_type in
    let r_tp   = T.Type.t_arrow (T.Scheme.of_type out_tp) res_tp res_eff in
    let (env, x) = Env.add_mono_var env x in_tp in
    let (env, r) = Env.add_mono_var env r r_tp in
    let (body, _) = check_expr_type env body res_tp res_eff in
    (make (T.HEffect(in_tp, out_tp, x, r, body)),
      T.Type.t_arrow (T.Scheme.of_type in_tp) out_tp
        (T.Effect.singleton h_eff))

(* ------------------------------------------------------------------------- *)
(** Check expression put into REPL *)
and check_repl_expr env e eff =
  let (e, tp, r_eff) = infer_expr_type env e eff in
  (e, "?", r_eff)
