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
      Env.t -> ImplicitEnv.t -> (T.typ, 'dir) request -> T.effrow ->
        T.expr * (T.typ, 'dir) response * ret_effect
  }

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

(** Check if given expression is a monomorphic variable *)
let is_monomorphic_var env (e : S.expr) =
  match e.data with
  | EPoly(p, _, _) ->
    let sch_opt  =
      match p.data with
      | EVar x -> Option.map snd (Env.lookup_var env x)
      | EImplicit name ->
        Option.map (fun (_, sch, _) -> sch) (Env.lookup_implicit env name)
      | ECtor _ | EMethod _ -> None
    in
    begin match sch_opt with
    | Some { sch_targs = []; sch_named = []; sch_body = _ } -> true
    | _ -> false
    end
  | _ -> false

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
  translated polymorphic expression, its scheme, and the hints for the scheme
  instantiation. The context is a function that takes instantiation, its type
  and effect, and returns translated expression, its type and the effect. *)
let rec infer_poly_scheme env (e : S.poly_expr) eff =
  let pos = e.pos in
  let default_ctx e tp r_eff = (e, tp, r_eff) in
  match e.data with
  | EVar  x ->
    let (e, sch) = infer_var_scheme ~pos env x in
    (default_ctx, e, sch, T.TVar.Map.empty)
  | EImplicit n ->
    let (e, sch) = infer_implicit_scheme ~pos env n in
    (default_ctx, e, sch, T.TVar.Map.empty)
  | ECtor c ->
    let (e, sch) = infer_ctor_scheme ~pos env c in
    (default_ctx, e, sch, T.TVar.Map.empty)
  | EMethod(self, name) ->
    let (self, self_tp, self_r_eff) = infer_expr_type env self eff in
    begin match T.Type.whnf self_tp with
    | Whnf_Neutral(NH_Var a, _) ->
      begin match Env.lookup_method env a name with
      | Some(x, sch) ->
        (method_call_ctx ~pos ~env ~self ~self_tp ~self_r_eff ~eff,
          { T.pos = pos; T.data = T.EVar x },
          sch,
          TypeUtils.method_inst_hints sch self_tp)
      | None ->
        Error.fatal (Error.unbound_method ~pos ~env a name)
      end
    | Whnf_Neutral(NH_UVar _, _) ->
      Error.fatal (Error.method_call_on_unknown_type ~pos:e.pos)

    | Whnf_PureArrow _ | Whnf_Arrow _ | Whnf_Handler _
    | Whnf_Label _ ->
      Error.fatal (Error.method_call_on_invalid_type ~pos:e.pos ~env self_tp)

    | Whnf_Effect _ | Whnf_Effrow _ ->
      failwith "Internal kind error"
    end

(** Context of a method call, returned by [infer_poly_scheme] *)
and method_call_ctx ~pos ~env ~self ~self_tp ~self_r_eff ~eff =
  fun e method_tp r_eff ->
  let result_expr = { T.pos = pos; T.data = T.EApp(e, self) } in
  match T.Type.view method_tp with
  | TArrow(
      { sch_targs = []; sch_named = []; sch_body = self_tp' },
      res_tp, res_eff) ->
    if not (Unification.subtype env self_tp self_tp') then
      Error.report
        (Error.expr_type_mismatch ~pos:self.pos ~env self_tp self_tp');
    if not (Unification.subeffect env res_eff eff) then
      Error.report (Error.method_effect_mismatch ~pos ~env res_eff eff);
    (* The method is impure, so the result is impure too. *)
    (result_expr, res_tp, Impure)
  | TPureArrow(
      { sch_targs = []; sch_named = []; sch_body = self_tp' },
      res_tp) ->
    if not (Unification.subtype env self_tp self_tp') then
      Error.report
        (Error.expr_type_mismatch ~pos:self.pos ~env self_tp self_tp');
    (result_expr, res_tp, ret_effect_join self_r_eff r_eff)
  | _ ->
    (* Method must be an arrow with monomorphic argument *)
    InterpLib.InternalError.report ~reason:"invalid method type" ()

(* ------------------------------------------------------------------------- *)
(** Infer type of an expression. The effect of an expression is always in
  the check mode. However, pure expressions may returns an information that
  they are pure (see [ret_effect] type). *)
and infer_expr_type env (e : S.expr) eff =
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

  | EStr s ->
    (make (T.EStr s), T.Type.t_var T.BuiltinType.tv_string, Pure)

  | EPoly(e, tinst, inst) ->
    let (p_ctx, e, sch, hints1) = infer_poly_scheme env e eff in
    Uniqueness.check_type_inst_uniqueness tinst;
    Uniqueness.check_inst_uniqueness inst;
    let tinst = Type.check_type_insts env tinst sch.sch_targs in
    let (hints2, cached_inst) =
      extract_implicit_type_hints ~pos env sch inst eff in
    let hints = TypeUtils.merge_hints hints1 hints2 in
    let (sub, tps) =
      ExprUtils.guess_types ~pos ~tinst ~hints env sch.sch_targs in
    let e = ExprUtils.make_tapp e tps in
    let named = List.map (T.NamedScheme.subst sub) sch.sch_named in
    let (i_ctx, inst, r_eff) =
      check_explicit_insts env named inst cached_inst eff in
    let e = ExprUtils.instantiate_named_params env e named inst in
    let tp = T.Type.subst sub sch.sch_body in
    (p_ctx (i_ctx e) tp r_eff)

  | EFn(arg, body) ->
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    let body_eff = Env.fresh_uvar env T.Kind.k_effrow in
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
    let (env, a) = Env.add_the_effect ~pos:e.pos env in
    let res_tp  = Env.fresh_uvar env T.Kind.k_type in
    let res_eff = Env.fresh_uvar env T.Kind.k_effrow in
    let (env, lx) =
      Env.add_the_label env (T.Type.t_var a) res_tp res_eff in
    let (h, tp, r_eff) = infer_expr_type env h T.Effect.pure in
    begin match r_eff with
    | Pure -> ()
    | Impure -> Error.report (Error.impure_handler ~pos:e.pos)
    end;
    let e = make (T.EHandler(a, lx, res_tp, res_eff, h)) in
    (e, T.Type.t_handler a tp res_tp res_eff, Pure)

  | EAnnot(e, tp) ->
    let tp = Type.tr_ttype env tp in
    let (e, r_eff) = check_expr_type env e tp eff in
    (e, tp, r_eff)

(* ------------------------------------------------------------------------- *)
(** Check type and effect of an expression. Returns also information about
  the purity of an expression. *)
and check_expr_type env (e : S.expr) tp eff =
  let make data = { e with data = data } in
  let pos = e.pos in
  match e.data with
  | EUnit | ENum _ | EStr _ | EPoly _ | EApp _ ->
    check_expr_type_default env e tp eff

  | EFn(arg, body) ->
    begin match Unification.from_arrow env tp with
    | Arr_UVar ->
      check_expr_type_default env e tp eff

    | Arr_Pure(sch, tp2) ->
      let (env, pat, r_eff1) = Pattern.check_arg_scheme env arg sch in
      let (body, r_eff2) = check_expr_type env body tp2 T.Effect.pure in
      if ret_effect_join r_eff1 r_eff2 <> Pure then
        Error.report (Error.func_not_pure ~pos);
      let (x, body) = ExprUtils.arg_match pat body tp2 T.Effect.pure in
      (make (T.EPureFn(x, sch, body)), Pure)

    | Arr_Impure(sch, tp2, eff) ->
      let (env, pat, _) = Pattern.check_arg_scheme env arg sch in
      let (body, _) = check_expr_type env body tp2 eff in
      let (x, body) = ExprUtils.arg_match pat body tp2 eff in
      (make (T.EFn(x, sch, body)), Pure)

    | Arr_No ->
      Error.report (Error.expr_not_function_ctx ~pos ~env tp);
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
    let (e, e_tp, _) = infer_expr_type env e eff in
    let (cls, r_eff) = check_match_clauses env e_tp cls tp eff in
    (make (T.EMatch(e, cls, tp, eff)), r_eff)

  | EHandler h ->
    begin match Unification.from_handler env tp with
    | H_Handler(b, tp, tp0, eff0) ->
      let (env, a) = Env.add_the_effect ~pos env in
      let sub  = T.Subst.rename_to_fresh T.Subst.empty b a in
      let tp   = T.Type.subst sub tp in
      let tp0  = T.Type.subst sub tp0 in
      let eff0 = T.Type.subst sub eff0 in
      let (env, l) = Env.add_the_label env (T.Type.t_var a) tp0 eff0 in
      let (h, r_eff) = check_expr_type env h tp T.Effect.pure in
      begin match r_eff with
      | Pure -> ()
      | Impure -> Error.report (Error.impure_handler ~pos)
      end;
      let e = make (T.EHandler(a, l, tp0, eff0, h)) in
      (e, Pure)

    | H_No ->
      Error.fatal (Error.expr_not_handler_ctx ~pos ~env tp)
    end

  | EEffect(arg, body) ->
    begin match Env.lookup_the_label env with
    | Some(lx, l_eff0, res_tp, res_eff) ->
      let l_eff = T.Type.t_closed_effrow (T.Type.effect_view l_eff0) in
      if not (Unification.subeffect env l_eff eff) then
        Error.report (Error.expr_effect_mismatch ~pos ~env l_eff eff);
      let r_tp = T.Type.t_arrow (T.Scheme.of_type tp) res_tp res_eff in
      let (env, rpat, _) =
        Pattern.check_arg_scheme env arg (T.Scheme.of_type r_tp) in
      let (body, _) = check_expr_type env body res_tp res_eff in
      let (x, body) = ExprUtils.arg_match rpat body res_tp res_eff in
      let e = make (T.EEffect(make (T.EVar lx), x, body, tp)) in
      (e, Impure)

    | None ->
      Error.fatal (Error.unbound_the_label ~pos)
    end

  | EExtern name ->
    (make (T.EExtern(name, tp)), Pure)

  | EAnnot(e', tp') ->
    let tp' = Type.tr_ttype env tp' in
    if not (Unification.subtype env tp' tp) then
      Error.report (Error.expr_type_mismatch ~pos ~env tp' tp);
    check_expr_type env e' tp' eff

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
  type dir. Env.t -> S.expr -> (T.typ, dir) request -> T.effrow ->
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
(** Extracts type hints for a type scheme from environment and explicit
  instantiation. Returns hints and cached translated explicit
  instantiations. *)
and extract_implicit_type_hints ~pos env (sch : T.scheme) inst eff =
  let targs = sch.sch_targs in
  let inst =
    List.map (fun { T.data = (n, e); _ } -> (Name.tr_name env n, e)) inst in
  let rec loop hints cache named =
    match named with
    | [] -> (hints, cache)
    | (name, arg_sch) :: named
        when T.Scheme.is_monomorphic arg_sch
        && TypeUtils.is_tvar_neutral arg_sch.sch_body ->
      let expr_opt =
        match T.Name.assoc name inst with
        | None ->
          let make data = { S.pos = pos; S.data = data } in
          begin match name with
          | NLabel | NVar _ -> None
          | NImplicit n ->
            Some (make (S.EPoly(make (S.EImplicit (S.NPName n)), [], [])))
          end
        | Some e -> Some e
      in
      begin match expr_opt with
      | None -> loop hints cache named
      | Some e ->
        let (e, tp, r_eff) = infer_expr_type env e eff in
        let hints2 = TypeUtils.type_inst_hints targs arg_sch.sch_body tp in
        loop
          (TypeUtils.merge_hints hints hints2)
          ((name, (e, tp, r_eff)) :: cache)
          named
      end
    | _ :: named ->
      loop hints cache named
  in
  loop T.TVar.Map.empty [] sch.sch_named

(* ------------------------------------------------------------------------- *)
(** Check explicit instantiations against given list of named parameters (from
  type scheme). It returns context (represented as meta-function) that
  preserves the order of computations, list of checked instantiations, and
  the effect. *)
and check_explicit_insts env named insts cache eff =
  match insts with
  | [] -> (Fun.id, [], Pure)
  | { data = (n, e); pos } :: insts ->
    let make data = { T.data; T.pos } in
    let n = Name.tr_name env n in
    let (e, sch, r_eff1) =
      match T.Name.assoc n named with
      | Some sch ->
        begin match T.Name.assoc n cache with
        | None ->
          let (e, r_eff1) = check_actual_arg env e sch eff in
          (e, sch, r_eff1)
        | Some (e, tp, r_eff1) ->
          assert (T.Scheme.is_monomorphic sch);
          if not (Unification.subtype env tp sch.sch_body) then
            Error.report
              (Error.named_param_type_mismatch ~pos:e.pos ~env n
                tp sch.sch_body);
          (e, sch, r_eff1)
        end
      | None ->
        Error.warn (Error.redundant_named_parameter ~pos n);
        let (e, tp, r_eff1) = infer_expr_type env e eff in
        (e, T.Scheme.of_type tp, r_eff1)
    in
    let (ctx, insts, r_eff2) =
      check_explicit_insts env named insts cache eff in
    let x = Var.fresh () in
    let ctx e0 = make (T.ELet(x, sch, e, ctx e0)) in
    let insts = (n, make (T.EVar x)) :: insts in
    (ctx, insts, ret_effect_join r_eff1 r_eff2)

(* ------------------------------------------------------------------------- *)
(** Check type and effect of a block of definitions. It uses bidirectional
  type checking, and pass the extended environment to the body-generating
  continuation. *)
and check_defs : type dir.
  Env.t -> ImplicitEnv.t -> S.def list ->
    (T.typ, dir) request -> T.effrow -> def_cont ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun env ienv defs req eff cont ->
  match defs with
  | [] -> cont.run env ienv req eff
  | def :: defs ->
    check_def env ienv def req eff
      { run = fun env ienv req eff -> check_defs env ienv defs req eff cont }

and check_def : type dir.
  Env.t -> ImplicitEnv.t -> ?public:bool -> S.def ->
    (T.typ, dir) request -> T.effrow -> def_cont ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun env ienv ?(public=false) def req eff cont ->
  let make (e : T.expr) data =
    { T.pos  = Position.join def.pos e.pos;
      T.data = data
    } in
  let pos = def.pos in
  match def.data with
  | DLetId(id, e1) ->
    let (sch, e1, r_eff1) = check_let ~pos env ienv e1 eff in
    let (env, ienv, x) =
      ImplicitEnv.add_poly_id ~pos env ienv ~public id sch in
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
    let (tvars1, ims1) = ImplicitEnv.end_generalize_pure ims1 in
    let (body, sch) =
      ExprUtils.generalize ~pos env (tvars1 @ tvars2) (ims1 @ ims2) body tp in
    let (env, ienv, x) =
      ImplicitEnv.add_poly_id ~pos env ienv ~public id sch in
    let (e2, resp, r_eff) = cont.run env ienv req eff in
    (make e2 (T.ELet(x, sch, body, e2)), resp, r_eff)

  | DLetPat(pat, e1) ->
    let (env1, ims) = ImplicitEnv.begin_generalize env ienv in
    let scope = Env.scope env1 in
    let (e1, tp, r_eff1) = infer_expr_type env1 e1 eff in
    ImplicitEnv.end_generalize_impure ims;
    let (env, pat, names, r_eff2) =
      Pattern.check_type ~env ~scope ~public pat tp in
    let ienv = ImplicitEnv.shadow_names ienv names in
    let (e2, resp, r_eff3) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    let res_tp = bidir_result req resp in
    (make e2 (T.EMatch(e1, [(pat, e2)], res_tp, eff)), resp,
      ret_effect_joins [ r_eff1; r_eff2; r_eff3 ])

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
      Pattern.check_type ~env ~scope:scope1 ~public pat l_tp in
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
        ImplicitEnv.end_generalize_impure ims;
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
    ImplicitEnv.end_generalize_impure ims;
    begin match Unification.to_handler env eh_tp with
    | H_Handler(a, cap_tp, res_tp, res_eff) ->
      let sub = lbl.l_sub a in
      let cap_tp  = T.Type.subst sub cap_tp  in
      let res_tp  = T.Type.subst sub res_tp  in
      let res_eff = T.Type.subst sub res_eff in
      if not (Unification.unify_type env lbl.l_delim_tp res_tp) then
        Error.report (Error.delim_type_mismatch
          ~pos:def.pos ~env lbl.l_delim_tp res_tp);
      if not (Unification.unify_type env lbl.l_delim_eff res_eff) then
        Error.report (Error.delim_effect_mismatch
          ~pos:def.pos ~env lbl.l_delim_eff res_eff);
      let (env, pat, names, _) =
        Pattern.check_type ~env ~scope:(Env.scope env) ~public pat cap_tp in
      let ienv = ImplicitEnv.shadow_names ienv names in
      let (ret_x, body_tp, ret_body) =
        check_return_clauses env rcs res_tp res_eff in
      let body_eff = T.Effect.cons_eff lbl.l_eff res_eff in
      let (body, Checked, _) = cont.run env ienv (Check body_tp) body_eff in
      let (x, body) = ExprUtils.arg_match pat body body_tp body_eff in
      let pos = Position.join def.pos body.pos in
      if not (Unification.subeffect env0 res_eff eff) then
        Error.report (Error.expr_effect_mismatch ~pos ~env:env0 res_eff eff);
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
      let (e, tp, r_eff) = check_finally_clauses env_f fcs e res_tp req eff in
      (lbl.l_ctx e, tp, r_eff)

    | H_No ->
      Error.fatal (Error.expr_not_handler ~pos:eh.pos ~env eh_tp)
    end

  | DImplicit(n, args, sch) ->
    let (env1, args1) = Type.tr_named_type_args env args in
    let sch  = Type.tr_scheme env1 sch in
    let args2 =
      T.UVar.Set.diff (T.Scheme.uvars sch) (Env.uvars env)
      |> T.UVar.Set.elements
      |> List.map (fun x -> (T.TNAnon, T.UVar.fix x))
    in
    let args = args1 @ args2 in
    let ienv = ImplicitEnv.declare_implicit ienv n args sch in
    cont.run env ienv req eff

  | DData dd ->
    let scope = Env.scope env in
    let (env, dd) = DataType.check_data_def env ~public dd in
    let (e, resp, r_eff) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EData([dd], e)), resp, r_eff)

  | DDataRec dds ->
    let scope = Env.scope env in
    let (env, dds) = DataType.check_rec_data_defs env ~public dds in
    let (e, resp, r_eff) = cont.run env ienv req eff in
    let resp = type_resp_in_scope ~env ~pos:def.pos ~scope resp in
    (make e (T.EData(dds, e)), resp, r_eff)

  | DModule(name, defs) ->
    let env = Env.enter_module env in
    check_defs env ImplicitEnv.empty defs req eff
      { run = fun env _ req eff ->
        let env = Env.leave_module env ~public name in
        cont.run env ienv req eff }

  | DOpen path ->
    begin match Env.lookup_module env path with
    | Some m ->
      let env = Env.open_module env ~public m in
      cont.run env ienv req eff
    | None ->
      Error.report (Error.unbound_module ~pos path);
      cont.run env ienv req eff
    end

  | DPub def -> check_def env ienv ~public:true def req eff cont

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
and check_let ~pos env ienv body eff =
  let (body_env, ims) = ImplicitEnv.begin_generalize env ienv in
  let (body, tp, r_eff) = infer_expr_type body_env body eff in
  (* Purity restriction: check if r_eff is pure. If so, then generalize type
    of an expression *)
  match r_eff with
  | Pure ->
    let (targs, ims) = ImplicitEnv.end_generalize_pure ims in
    (* TODO: make sure that names do not overlap *)
    let (body, sch) = ExprUtils.generalize ~pos env targs ims body tp in
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

and make_nonempty_match env tp cls res_tp res_eff =
  let pos =
    match cls with
    | [] -> assert false
    | cl :: cls ->
      let p1 = cl.S.pos in
      let p2 = List.fold_left (fun _ cl -> cl.S.pos) p1 cls in
      Position.join p1 p2
  in
  let (cls, _) = check_match_clauses env tp cls res_tp res_eff in
  let x = Var.fresh () in
  let body =
    { T.pos;
      T.data = T.EMatch({ pos; data = T.EVar x }, cls, res_tp, res_eff) }
  in (x, body)

(* ------------------------------------------------------------------------- *)
(** Check return clauses of handler.
  In [check_return_clauses env rcs res_tp res_eff] the meaning of the
  parameters is the following.
  - [env]     -- the environment.
  - [rcs]     -- list of clauses. If this list is empty, then the default
      identity clause is created.
  - [res_tp]  -- the expected of the return clause
  - [ref_eff] -- the expected effect of the return clause

  This function returns triple: a variable bound by the return clause,
  its type, and the body of the clause (including pattern-matching). *)
and check_return_clauses env rcs res_tp res_eff =
  match rcs with
  | [] ->
    let x = Var.fresh () in
    (x, res_tp, { T.pos = Position.nowhere; T.data = T.EVar x })
  | _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let (x, body) = make_nonempty_match env tp rcs res_tp res_eff in
    (x, tp, body)

(* ------------------------------------------------------------------------- *)
(** Check finally clauses of handler.
  In [check_finally_clauses env fcs hexpr htp req eff] the meaning of the
  parameters is the following.
  - [env]   -- the environment.
  - [fcs]   -- list of clauses. If this list is empty, then the equivalent of
      the default identity clause is created.
  - [hexpr] -- the handler expression, to be wrapped around the finally
      clauses.
  - [htp]   -- the type of the handler expression
  - [req]   -- type request of the bidirectional type-checking.
  - [eff]   -- the expected effect of the clauses.

  This function returns a triple with the same meaning as the triple returned
  by [tr_expr] function. Handlers are always impure. *)
and check_finally_clauses : type dir.
  Env.t -> S.match_clause list -> T.expr -> T.typ ->
    (T.typ, dir) request -> T.effrow ->
      T.expr * (T.typ, dir) response * ret_effect =
  fun env fcs hexpr htp req eff ->
  match fcs with
  | [] ->
    begin match req with
    | Infer -> (hexpr, Infered htp, Impure)
    | Check tp ->
      if not (Unification.subtype env htp tp) then
        Error.report (Error.expr_type_mismatch ~pos:hexpr.pos ~env htp tp);
      (hexpr, Checked, Impure)
    end
  | _ ->
    let (tp, (resp : (T.typ, dir) response)) =
      match req with
      | Infer ->
        let tp = Env.fresh_uvar env T.Kind.k_type in
        (tp, Infered tp)
      | Check tp -> (tp, Checked)
    in
    let (x, body) = make_nonempty_match env htp fcs tp eff in
    let expr =
      { T.pos  = Position.join body.pos hexpr.pos;
        T.data = T.ELet(x, T.Scheme.of_type htp, hexpr, body) }
    in (expr, resp, Impure)

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
  let pp_ctx = Pretty.empty_context () in
  (e, Pretty.type_to_string pp_ctx env tp, r_eff)
