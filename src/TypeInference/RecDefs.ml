(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for recursive definitions *)

open Common
open TypeCheckFix

(* TODO: this piece of code is copied from `Def.ml`. Because we are going to
  change the design of handlers and labels, I don't care of code duplication
  here. *)
let add_label_effect env eff_opt =
  match eff_opt with
  | None     -> Env.add_the_effect env
  | Some arg -> Type.check_type_arg env arg T.Kind.k_effect

(* ========================================================================= *)
(** The first pass: extending the environment by recursive data and effects
  (attached to labels) *)

(** Representation of recursive definition after the first pass *)
type def1 =
  | D1_Blank
  | D1_LetVar  of S.ident * S.scheme_expr * S.expr
  | D1_LetFun  of S.ident * S.named_type_arg list * S.named_arg list * S.expr
  | D1_Label   of T.tvar * S.ident * (Position.t * S.scheme_expr) option
  | D1_Data    of T.tvar * S.tvar * S.named_type_arg list * S.ctor_decl list
  | D1_Section of def1 T.node list

(** The main function of the first pass. *)
let rec prepare_rec_data env (def : S.def) =
  let make data = { def with data = data } in
  let pos = def.pos in
  match def.data with
  | DLetId(id, e) ->
    (env, make (D1_LetFun(id, [], [], e)))
  | DLetFun(id, targs, nargs, body) ->
    (env, make (D1_LetFun(id, targs, nargs, body)))
  | DLetPat(pat, body) ->
    begin match pat.data with
    | PAnnot({ data = PId id; _ }, sch) ->
      (env, make (D1_LetVar(id, sch, body)))
    | _ ->
      Error.fatal (Error.invalid_rec_def ~pos:def.pos)
    end
  | DMethodFn(public, x, method_name) ->
    let env = Env.add_method_fn ~public env x method_name in
    (env, make D1_Blank)
  | DLabel(eff_opt, pat) ->
    let (env, l_eff) = add_label_effect env eff_opt in
    let (id, tp_opt) =
      begin match pat.data with
      | PId id -> (id, None)
      | PAnnot({ data = PId id; _ }, sch) ->
        (id, Some(pat.pos, sch))
      | _ ->
        Error.fatal (Error.invalid_rec_def ~pos:def.pos)
      end
    in
    (env, make (D1_Label(l_eff, id, tp_opt)))
  | DImplicit(n, args, sch) ->
    (* TODO: They could be recursive, but their semantics needs to be
      discussed. *)
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)
  | DData(public, name, args, ctors) ->
    let (_, args') = Type.tr_named_type_args env args in
    let kind  = DataType.kind args' in
    let (env, x) = Env.add_tvar ~pos env ~public name kind in
    (env, make (D1_Data(x, name, args, ctors)))
  | DRec defs ->
    let (env, defs) = List.fold_left_map prepare_rec_data env defs in
    (env, make (D1_Section defs))
  | DModule(pub, name, defs) ->
    (* TODO: recursive modules are not supported, yet *)
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)
  | DOpen(pub, path) ->
    (* TODO: recursive modules are not supported, yet *)
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DHandlePat _ ->
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)
  | DReplExpr _ ->
    assert false

(* ========================================================================= *)
(** The second pass: finalization of checking labels and ADTs, i.e., extending
  the environment by term-level information (labels and proofs). *)

(** The second pass for single definition. See [finalize_rec_data_defs] for
  more details. *)
let rec finalize_rec_data env ienv (def : def1 T.node) =
  match def.data with
  | D1_Blank | D1_LetVar _ | D1_LetFun _ -> (env, ienv, [], Pure)
  | D1_Label(tvar, id, tp_opt) ->
    let delim_tp  = Env.fresh_uvar env T.Kind.k_type in
    let delim_eff = Env.fresh_uvar env T.Kind.k_effrow in
    let l_tp = T.Type.t_label (T.Type.t_var tvar) delim_tp delim_eff in
    begin match tp_opt with
    | None -> ()
    | Some(pat_pos, sch') ->
      let sch' = Type.tr_scheme env sch' in
      let sch = T.Scheme.of_type l_tp in
      Error.check_unify_result ~pos:pat_pos
        (Unification.subscheme env sch sch')
        ~on_error:(Error.pattern_annot_mismatch ~env sch sch')
    end;
    let (env, ienv, var) =
      ImplicitEnv.add_mono_id ~pos:def.pos env ienv id l_tp in
    let dd = T.DD_Label { tvar; var; delim_tp; delim_eff } in
    (env, ienv, [dd], Impure)

  | D1_Data(x, name, args, ctors) ->
    let (data_env, args) = Type.tr_named_type_args env args in
    begin match
      Unification.unify_kind (DataType.kind args) (T.TVar.kind x)
    with
    | true -> ()
    | false -> assert false
    end;
    let ctors = DataType.check_ctor_decls ~data_targs:args data_env ctors in
    let (env, dd) = DataType.finalize_check env x ~name args ctors in 
    (env, ienv, [dd], Pure)
  | D1_Section defs ->
    let (env, dds, r_eff) = finalize_rec_data_defs env ienv defs in
    (env, ienv, dds, r_eff)

(** The main function of the second pass. It returns extended environment,
  list of data/label definitions and the effect of this block (impure, when
  new run-time labels are generated). *)
and finalize_rec_data_defs env ienv defs =
  match defs with
  | [] -> (env, [], Pure)
  | def :: defs ->
    let (env, ienv, dds1, r_eff1) = finalize_rec_data env ienv def in
    let (env, dds2, r_eff2) = finalize_rec_data_defs env ienv defs in
    (env, dds1 @ dds2, ret_effect_join r_eff1 r_eff2)

(* ========================================================================= *)
(** The third pass: Guessing the schemes of recursive definitions *)

(** Representation of recursive definition after the third pass *)
type def3 =
  | D3_LetVar of {
      var    : T.var;
      scheme : T.scheme;
      name   : S.ident;
      body   : S.expr
    }
  | D3_LetFun of {
      var    : T.var;
      scheme : T.scheme;
      name   : S.ident;
      targs  : S.named_type_arg list;
      nargs  : S.named_arg list;
      body   : S.expr
    }
  | D3_Section of def3 T.node list

(** Guess the type of a recursive value *)
let guess_rec_value_type body_env (body : S.expr) =
  (* TODO: this function could be better *)
  Env.fresh_uvar body_env T.Kind.k_type

(** Information gathered by guessing the scheme of a recursive value *)
type rec_value_scheme_info =
  { rsi_scheme  : T.scheme;
      (** Type scheme of a recursive value *)

    rsi_tvars   : T.named_tvar list;
      (** Explicit formal type parameters *)

    rsi_named   : (T.name * T.pattern * T.scheme) list;
      (** Explicit named formal parameters *)

    rsi_arg_eff : ret_effect;
      (** Effect of pattern-matching of named parameters *)
    
    rsi_env     : Env.t;
      (** Environment extended with explicit formal parameters *)

    rsi_body_tp : T.typ
      (** Guessed type of a body *)
  }

(** Check formal parameters and guess the scheme of a recursive value *)
let rec_value_scheme env targs nargs body =
  let (env, tvars) = Type.tr_named_type_args env targs in
  let (env, named, r_eff) =
    Pattern.infer_named_arg_schemes env nargs in
  let body_tp = guess_rec_value_type env body in
  let sch =
    { T.sch_targs = tvars;
      T.sch_named = List.map (fun (name, _, sch) -> (name, sch)) named;
      T.sch_body  = body_tp
    } in
  { rsi_scheme  = sch;
    rsi_tvars   = tvars;
    rsi_named   = named;
    rsi_arg_eff = r_eff;
    rsi_env     = env;
    rsi_body_tp = body_tp
  }

(** The third pass for a single definition. It returns an option type: value
  [None] means that the definition does not produce any recursive value. *)
let rec prepare_rec_fun env ienv (def : def1 T.node) =
  match def.data with
  | D1_Blank | D1_Label _ | D1_Data _ -> None
  | D1_LetVar(name, sch, body) ->
    let sch = Type.tr_scheme env sch in
    let (env, ienv, x) =
      ImplicitEnv.add_poly_id ~pos:def.pos env ienv name sch in
    let def = 
      { def with data = D3_LetVar { var = x; scheme = sch; name; body } }
    in
    Some (env, ienv, def)
  | D1_LetFun(name, targs, nargs, body) ->
    let sch = (rec_value_scheme env targs nargs body).rsi_scheme in
    let (env, ienv, x) =
      ImplicitEnv.add_poly_id ~pos:def.pos env ienv name sch in
    let def =
      { def with
        data = D3_LetFun { var = x; scheme = sch; name; targs; nargs; body }
      } in
    Some (env, ienv, def)
  | D1_Section defs ->
    let (env, defs) = prepare_rec_funs env ienv defs in
    let def = { def with data = D3_Section defs } in
    Some (env, ienv, def)

(** The main function of the third pass *)
and prepare_rec_funs env ienv defs =
  match defs with
  | [] -> (env, [])
  | def :: defs ->
    begin match prepare_rec_fun env ienv def with
    | None -> prepare_rec_funs env ienv defs
    | Some(env, ienv, def) ->
      let (env, defs) = prepare_rec_funs env ienv defs in
      (env, def :: defs)
    end

(* ========================================================================= *)
(** The fourth pass: actual type-checking *)

(** Representation of recursive definition after the fourth pass *)
type def4 =
  | D4_LetFun  of T.var * T.scheme * S.ident * T.named_tvar list * T.expr
  | D4_Section of def4 T.node list

(** The main function of the fourth pass. *)
let rec check_rec_fun ~tcfix env (def : def3 T.node) =
  let open (val tcfix : TCFix) in
  match def.data with
  | D3_LetVar vd ->
    let (env, tvs, named, tp) =
      TypeUtils.open_scheme ~pos:def.pos env vd.scheme in
    let (body, r_eff) = check_expr_type env vd.body tp T.Effect.pure in
    begin match r_eff with
    | Pure -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos:def.pos)
    end;
    let body = ExprUtils.make_nfun named body in
    { def with
      data = D4_LetFun(vd.var, vd.scheme, vd.name, tvs, body)
    }

  | D3_LetFun fd ->
    let sch_info = rec_value_scheme env fd.targs fd.nargs fd.body in
    if Unification.subscheme env fd.scheme sch_info.rsi_scheme <> Unify_Success
    then
      (* Both schemes come from the same definition. They should be unifiable *)
      assert false;
    let (body, r_eff2) =
      check_expr_type sch_info.rsi_env fd.body
        sch_info.rsi_body_tp T.Effect.pure in
    begin match ret_effect_join sch_info.rsi_arg_eff r_eff2 with
    | Pure -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos:def.pos)
    end;
    let (named, body) =
      ExprUtils.inst_args_match sch_info.rsi_named body
        sch_info.rsi_body_tp T.Effect.pure in
    let body = ExprUtils.make_nfun named body in
    { def with
      data = D4_LetFun(fd.var, fd.scheme, fd.name, sch_info.rsi_tvars, body)
    }

  | D3_Section defs ->
    let defs = List.map (check_rec_fun ~tcfix env) defs in
    { def with data = D4_Section defs }

(* ========================================================================= *)
(** The fifth pass: collecting unification variables *)

(** The main function of the fifth pass *)
let rec collect_rec_fun_uvars uvs (def : def4 T.node) =
  match def.data with
  | D4_LetFun(_, sch, _, _, _) ->
    T.Scheme.collect_uvars sch uvs
  | D4_Section defs ->
    List.fold_left collect_rec_fun_uvars uvs defs

(* ========================================================================= *)
(** The sixth pass: building final environment *)

(** Representation of recursive definition after the sixth pass *)
type def6 = {
  d6_pos         : Position.t;
  d6_poly_var    : T.var;
  d6_poly_scheme : T.scheme;
  d6_mono_var    : T.var;
  d6_mono_scheme : T.scheme;
  d6_mono_body   : T.expr;
  d6_tvars       : T.named_tvar list;
  d6_named       : (T.name * T.var * T.scheme) list;
  d6_body        : T.expr;
}

let make_app ~pos f ims =
  let make data = { T.pos = pos; T.data = data } in
  List.fold_left
    (fun f (_, x, _) -> make (T.EApp(f, make (T.EVar x))))
    f
    ims

let make_tapp ~pos f tvs =
  let make data = { T.pos = pos; T.data = data } in
  List.fold_left
    (fun f (_, x) -> make (T.ETApp(f, T.Type.t_var x)))
    f
    tvs

(** The sixth pass for a single definition *)
let rec add_rec_fun env ienv tvars1 ims1 (def : def4 T.node) =
  match def.data with
  | D4_LetFun(x, sch, name, tvars2, body) ->
    let make data = { def with data = data } in
    let make_app  ims f = make_app ~pos:def.pos f ims in
    let make_tapp tvs f = make_tapp ~pos:def.pos f tvs in
    (* New, more general scheme *)
    let y_sch =
      { T.sch_targs = tvars1 @ sch.T.sch_targs;
        T.sch_named =
          List.map (fun (name, _, sch) -> (name, sch)) ims1 @ sch.T.sch_named;
        T.sch_body  = sch.T.sch_body
      } in
    let (env, ienv, y) =
      ImplicitEnv.add_poly_id ~pos:def.pos env ienv name y_sch in
    (* Less-generalized form of this function *)
    let x_body =
      ExprUtils.make_tfun tvars2
        (make (T.EVar y)
        |> make_tapp tvars1 |> make_tapp tvars2 |> make_app ims1) in
    let def = {
      d6_pos         = def.pos;
      d6_poly_var    = y;
      d6_poly_scheme = y_sch;
      d6_mono_var    = x;
      d6_mono_scheme = sch;
      d6_mono_body   = x_body;
      d6_tvars       = tvars1 @ tvars2;
      d6_named       = ims1;
      d6_body        = body
    } in
    (env, ienv, [def])

  | D4_Section defs ->
    add_rec_funs env ienv tvars1 ims1 defs

(** The main function of the sixth pass *)
and add_rec_funs env ienv tvars ims defs =
  match defs with
  | [] -> (env, ienv, [])
  | def :: defs ->
    let (env, ienv, defs1) = add_rec_fun env ienv tvars ims def in
    let (env, ienv, defs2) = add_rec_funs env ienv tvars ims defs in
    (env, ienv, defs1 @ defs2)

(* ========================================================================= *)
(** The seventh pass: update bodies of recursive definitions with local
  definitions of less-generalized functions *)

(** Build an expression with local definitions of less-generalized
  functions. *)
let update_rec_body (fds : def6 list) (fd : def6) =
  let build_local_inst fd' body =
    { T.pos = fd.d6_pos;
      T.data =
        T.ELet(fd'.d6_mono_var, fd'.d6_mono_scheme, fd'.d6_mono_body, body)
    } in
  let build_local_ctx body =
    List.fold_right build_local_inst fds body in
  let rec update (body : T.expr) =
    let make data = { body with data = data } in
    match body.T.data with
    | EUnitPrf | ENum _ | EStr _ | EExtern _ -> body
    | EVar x ->
      if List.exists (fun fd -> Var.equal x fd.d6_mono_var) fds then
        Error.fatal (Error.non_productive_rec_def ~pos:body.pos);
      body
    | EFn(arg, arg_sch, body) ->
      make (T.EFn(arg, arg_sch, build_local_ctx body))
    | EPureFn(arg, arg_sch, body) ->
      make (T.EPureFn(arg, arg_sch, update body))
    | ETFun(x, body) ->
      make (T.ETFun(x, update body))
    | ECtor(prf, n, tps, args) ->
      make (T.ECtor(prf, n, tps, List.map update args))
    | EApp _ | ETApp _ | ELet _ | ELetRec _ | EData _ | EMatchEmpty _
    | EMatch _ | EHandle _ | EHandler _ | EEffect _ | ERepl _
    | EReplExpr _ ->
      Error.fatal (Error.non_productive_rec_def ~pos:body.pos)
  in
  update fd.d6_body

(** The main function of the seventh pass *)
let finalize_rec_fun (fds : def6 list) (fd : def6) =
  let body = update_rec_body fds fd in
  let body =
    ExprUtils.make_tfun fd.d6_tvars
      (ExprUtils.make_nfun fd.d6_named body) in
  (fd.d6_poly_var, fd.d6_poly_scheme, body)

(* ========================================================================= *)

let check_rec_defs ~tcfix env ienv defs =
  let (env, defs) = List.fold_left_map prepare_rec_data env defs in
  let (env, dds, r_eff) = finalize_rec_data_defs env ienv defs in
  let (rec_env, ims) = ImplicitEnv.begin_generalize env ienv in
  let (rec_env, fds) = prepare_rec_funs rec_env ienv defs in
  let fds = List.map (check_rec_fun ~tcfix rec_env) fds in
  let uvars = List.fold_left collect_rec_fun_uvars T.UVar.Set.empty fds in
  let (tvars, ims) = ImplicitEnv.end_generalize_pure ims uvars in
  let (env, ienv, fds) = add_rec_funs env ienv tvars ims fds in
  let fds = List.map (finalize_rec_fun fds) fds in
  (env, dds, fds, r_eff)
