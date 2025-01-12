(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

open Common

(** Pattern of a named argument *)
type named_pattern = {
          name : T.name;
          var  : T.var;
  mutable pat  : T.pattern option;
          sch  : T.scheme;
}

let select_named_type name tvars =
  tvars
  |> List.find_opt
      (fun (n, _) ->
        match n with
        | T.TNVar x -> x = name
        | T.TNAnon  -> false)
  |> Option.map snd

let select_named_pattern name named =
  List.find_opt (fun np -> T.Name.equal np.name name) named

let make_pattern ~pos np =
  let pat =
    match np.pat with
    | None     -> { T.pos; T.data = T.PWildcard np.sch }
    | Some pat -> pat
  in
  { pat with data = T.PAs(pat, np.var, np.sch) }

let make_arg_pattern np =
  Option.map (fun pat -> (np.var, pat)) np.pat

(* ========================================================================= *)

(** Add all named types to the environment. Returns extended environment. *)
let add_types_to_env ~pos env tvars =
  let add_type env (name, x) =
    match name with
    | T.TNAnon -> env
    | T.TNVar name -> Env.add_tvar_alias ~pos env name x
  in
  List.fold_left add_type env tvars

(** Create a partial environment that contains a single module with all named
  parameters. *)
let make_module_penv ~pos ~public ~env modname tvars named =
  let types =
    tvars |> List.filter_map
      (fun (name, x) ->
        match name with
        | T.TNAnon     -> None
        | T.TNVar name -> Some (name, x)) in
  let vars =
    named |> List.filter_map
      (fun { name; var; pat = _; sch } ->
        match name with
        | T.NVar x | T.NOptionalVar x -> Some (x, var, sch)
        | T.NImplicit _ | T.NMethod _ -> None) in
  let implicits =
    named |> List.filter_map
      (fun { name; var; pat = _; sch } ->
        match name with
        | T.NImplicit n -> Some (n, var, sch)
        | T.NVar _ | T.NOptionalVar _ | T.NMethod _ -> None) in
  let methods =
    named |> List.filter_map
      (fun { name; var; pat = _; sch } ->
        match name with
        | T.NMethod mname ->
          let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
          Some (owner, mname, var, sch)
        | T.NVar _ | T.NOptionalVar _ | T.NImplicit _ -> None) in
  PartialEnv.singleton_module ~public ~pos
    ~types ~vars ~implicits ~methods modname

(** Create a partial environment that contains all named parameters. *)
let make_open_penv ~pos ~public tvars named =
  let add_type penv (name, x) =
    match name with
    | T.TNAnon -> penv
    | T.TNVar name ->
      PartialEnv.add_tvar_alias ~public ~pos penv name x
  in
  let add_value penv { name; var; sch; pat = _ } =
    match name with
    | NVar x | NOptionalVar x ->
      PartialEnv.add_var ~public ~pos penv x var sch
    | NImplicit n ->
      PartialEnv.add_implicit ~public ~pos penv n var sch
    | NMethod _ ->
      (* Methods were added to the environment during the opening of the
        constructor. *)
      penv
  in
  let penv = List.fold_left add_type PartialEnv.empty tvars in
  List.fold_left add_value penv named

(* ========================================================================= *)

(** Introduce all existential types as anonymous type variables to the
  environment and freshly created partial environment. Returns both
  environments, list of refreshed named type variables, and a renaming
  substitution that can be used to refresh the types in the constructor. *)
let open_ctor_types ~pos env targs =
  let open_ctor_type (env, penv, sub) (tname, x) =
    let (env, y) = Env.add_anon_tvar ~pos env (T.TVar.kind x) in
    let penv = PartialEnv.add_anon_tvar ~pos penv y in
    let sub = T.Subst.rename_to_fresh sub x y in
    ((env, penv, sub), (tname, y))
  in
  let ((env, penv, sub), tvars) =
    List.fold_left_map open_ctor_type
      (env, PartialEnv.empty, T.Subst.empty)
      targs
  in
  (env, penv, tvars, sub)

(** Implicitly introduce all named arguments into the environment. Returns
  extended environment and a list of named arguments. *)
let open_ctor_named_args ~pos ~env penv named =
  let open_named_arg penv (name, sch) =
    let x = Var.fresh () in
    let penv =
      match name with
      | T.NVar _ | T.NOptionalVar _ | T.NImplicit _ -> penv
      | T.NMethod mname ->
        let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
        (* Should it be private? *)
        PartialEnv.add_method ~public:false ~pos ~env penv owner mname x sch
    in
    (penv, (name, x, sch))
  in
  List.fold_left_map open_named_arg penv named

(** Open a constructor: introduce all existential types as anonymous type
  variables to the environment, create a partial environment that contains
  all implicitly bound variables, and return a list of named type variables,
  initial list of named arguments, and refreshed list of arguments' schemes.
  *)
let open_ctor ~pos env (ctor : T.ctor_decl) =
  let (env, penv, tvars, sub) = open_ctor_types ~pos env ctor.ctor_targs in
  let named = List.map (T.NamedScheme.subst sub) ctor.ctor_named in
  let (penv, named) = open_ctor_named_args ~pos ~env penv named in
  let arg_schemes = List.map (T.Scheme.subst sub) ctor.ctor_arg_schemes in
  (env, penv, tvars, named, arg_schemes)

(** For a given constructor name and checked type, produce the ADT info for
    the type, the constructor index, proof that it is an ADT, and the needed
    substitution for the type parameters *)
let get_ctor_info ~pos env (cpath : S.ctor_name S.path) tp =
  let make data = { cpath with T.data = data } in
  match T.Type.whnf tp with
  | Whnf_Neutral(NH_Var x, rev_tps) ->
    begin match ModulePath.lookup_adt env cpath x with
    | Some info ->
      let tps = List.rev rev_tps in
      let sub_add sub (_, tv) tp = T.Subst.add_type sub tv tp in
      let sub = List.fold_left2 sub_add T.Subst.empty info.adt_args tps in
      let cname = S.path_name cpath in
      begin match T.CtorDecl.find_index info.adt_ctors cname with
      | Some idx ->
        let tps = List.map (fun tp -> make (T.TE_Type tp)) tps in
        let proof = make (T.EInst(info.adt_proof, tps, [])) in
        (info, idx, proof, sub)
      | None ->
        Error.fatal (Error.ctor_not_in_type ~pos:cpath.pos ~env cname tp)
      end
    | None ->
      Error.fatal (Error.ctor_pattern_on_non_adt ~pos ~env tp)
    end

  | Whnf_Neutral(NH_UVar _, _) ->
    let (idx, info) = ModulePath.lookup_ctor env cpath in
    let (sub, tps) = ParamResolve.guess_types ~pos env info.adt_args in
    let proof = make (T.EInst(info.adt_proof, tps, [])) in
    (info, idx, proof, sub)

  | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
    Error.fatal (Error.ctor_pattern_on_non_adt ~pos ~env tp)

  | Whnf_Effect ->
    failwith "Internal kind error"

(* ========================================================================= *)

let rec check_scheme env (pat : S.pattern) sch =
  let make data = { pat with T.data = data } in
  let pos = pat.pos in
  match pat.data with
  | PWildcard ->
    (PartialEnv.empty, make (T.PWildcard sch), T.Pure)

  | PId(public, id) ->
    let (penv, x) =
      match id with
      | IdVar      x -> PartialEnv.singleton_var ~public ~pos x sch
      | IdImplicit n -> PartialEnv.singleton_implicit ~public ~pos n sch
      | IdMethod name ->
        let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
        PartialEnv.singleton_method ~public ~pos owner name sch
    in
    (penv, make (T.PAs(make (T.PWildcard sch), x, sch)), T.Pure)

  | PCtor _ ->
    begin match T.Scheme.to_type sch with
    | Some tp -> check_type env pat tp
    | None -> Error.fatal (Error.non_polymorphic_pattern ~pos)
    end

  | PAnnot(pat, sch') ->
    let sch_expr = Type.tr_scheme env sch' in
    let sch' = T.SchemeExpr.to_scheme sch_expr in
    Error.check_unify_result ~pos:sch_expr.se_pos
      (Unification.subscheme env sch sch')
      ~on_error:(Error.pattern_annot_mismatch ~env sch sch');
    check_scheme env pat sch'

and check_type env (pat : S.pattern) tp =
  let make data = { pat with T.data = data } in
  let pos = pat.pos in
  match pat.data with
  | PWildcard | PId _ | PAnnot _ ->
    let sch = T.Scheme.of_type tp in
    check_scheme env pat sch

  | PCtor(cpath, named_pats, pats) ->
    let (info, idx, proof, sub) = get_ctor_info ~pos env cpath tp in
    let ctor = T.CtorDecl.subst sub (List.nth info.adt_ctors idx) in
    let res_tp = T.Type.subst sub info.adt_type in
    Error.check_unify_result ~pos (Unification.subtype env tp res_tp)
      ~on_error:(Error.pattern_type_mismatch ~env res_tp tp);
    let (env, penv, tvars, named, arg_schemes) = open_ctor ~pos env ctor in
    let (env, penv, named, eff1) =
      check_named_patterns ~pos env penv named_pats tvars named in
    let ps1 = List.map (make_pattern ~pos) named in
    if List.length arg_schemes <> List.length pats then
      Error.fatal (Error.ctor_arity_mismatch ~pos:pat.pos
        cpath (List.length arg_schemes) (List.length pats));
    let (penv, ps2, eff2) =
      check_patterns env penv pats arg_schemes in
    let cname = S.path_name cpath in
    let pat = make
      (T.PCtor(cname, idx, proof, List.map snd tvars, ps1, ps2)) in
    let eff = T.Effect.joins [info.adt_effect; eff1; eff2] in
    (penv, pat, eff)

and check_patterns env penv pats schs =
  match pats, schs with
  | [], [] -> (penv, [], T.Pure)
  | pat :: pats, sch :: schs ->
    let (penv1, pat, eff1) = check_scheme env pat sch in
    let (penv, pats, eff2) =
      check_patterns env (PartialEnv.join ~env penv penv1) pats schs in
    (penv, pat :: pats, T.Effect.join eff1 eff2)

  | [], _ :: _ | _ :: _, [] -> assert false

and check_named_patterns ~pos env penv named_pats tvars named =
  let prepare_named (name, x, sch) =
    { name; var = x; pat = None; sch } in
  let named = List.map prepare_named named in
  let rec loop env penv named_pats eff =
    match named_pats with
    | [] -> (env, penv, named, eff)
    | np :: named_pats ->
      let (env, penv1, eff1) = check_named_pattern env np tvars named in
      let penv = PartialEnv.join ~env penv penv1 in
      let eff  = T.Effect.join eff eff1 in
      loop env penv named_pats eff
  in
  loop env penv named_pats T.Pure

and check_named_pattern env np tvars named =
  match np.data with
  | NP_Type(_, { T.data = (TNAnon, _); _ }) ->
    Error.report (Error.anonymous_type_pattern ~pos:np.pos);
    (env, PartialEnv.empty, T.Pure)

  | NP_Type(public, { T.data = (TNVar name, arg); T.pos = pos }) ->
    begin match select_named_type name tvars, arg.data with
    | Some x, TA_Wildcard ->
      (env, PartialEnv.empty, T.Pure)

    | Some x, TA_Var(name, kind) ->
      let kind = Type.tr_kind kind in
      let x_kind = T.TVar.kind x in
      if not (Unification.unify_kind x_kind kind) then
        Error.fatal (Error.kind_annot_mismatch ~pos x_kind kind);
      let env = Env.add_tvar_alias ~pos env name x in
      let penv = PartialEnv.singleton_tvar_alias ~public ~pos name x in
      (env, penv, T.Pure)

    | None, _ ->
      Error.report (Error.unknown_named_type_pattern ~pos name);
      (env, PartialEnv.empty, T.Pure)
    end

  | NP_Val((NVar _ | NOptionalVar _ | NImplicit _) as name, pat) ->
    let name = tr_name name in
    begin match select_named_pattern name named with
    | None ->
      Error.report (Error.unknown_named_pattern ~pos:np.pos name);
      (env, PartialEnv.empty, T.Pure)

    | Some { pat = Some ppat; sch; _ } ->
      let ppos = ppat.pos in
      Error.report (Error.multiple_named_patterns ~pos:np.pos ~ppos name);
      let (penv, _, eff) = check_scheme env pat sch in
      (env, penv, eff)

    | Some ({ pat = None; sch; _ } as np) ->
      let (penv, pat, eff) = check_scheme env pat sch in
      np.pat <- Some pat;
      (env, penv, eff)
    end

  | NP_Val(NMethod _, _) ->
    Error.report (Error.method_pattern_not_allowed ~pos:np.pos);
    (env, PartialEnv.empty, T.Pure)

  | NP_Module modname ->
    (* TODO: Public flag is missing *)
    (* Since during type-checking of patterns we use the environment to
      store types, we don't need to add regular variables. *)
    let env = Env.enter_module env in
    let env = add_types_to_env ~pos:np.pos env tvars in
    let env = Env.leave_module env ~public:false modname in
    let penv =
      make_module_penv ~pos:np.pos ~public:false ~env modname tvars named in
    (env, penv, T.Pure)

  | NP_Open ->
    (* TODO: Public flag is missing *)
    (* Since during type-checking of patterns we use the environment to
      store types, we don't need to add regular variables. *)
    let env = add_types_to_env ~pos:np.pos env tvars in
    let penv = make_open_penv ~pos:np.pos ~public:false tvars named in
    (env, penv, T.Pure)

(* ========================================================================= *)

let infer_scheme env (pat : S.pattern) =
  match pat.data with
  | PWildcard | PId _ | PCtor _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let sch = T.Scheme.of_type tp in
    let (penv, pat, eff) = check_type env pat tp in
    (penv, pat, sch, eff)

  | PAnnot(pat, sch) ->
    let sch_expr = Type.tr_scheme env sch in
    let sch = T.SchemeExpr.to_scheme sch_expr in
    let (penv, pat, eff) = check_scheme env pat sch in
    let pat = { pat with T.data = T.PAnnot(pat, sch_expr) } in
    (penv, pat, sch, eff)

(** Translate a named pattern. Returns the tuple of the following:
  - environment extended with type parameters, but not with existential types
  - partial environment with all variables introduced by the pattern
  - list of named type variables (with external names) introduced by this
    pattern
  - list of named arguments introduced by this pattern
  - the effect of the pattern-matching *)
let infer_named_pattern env (np : S.named_pattern) =
  match np.data with
  | NP_Type(public, { data = (name, arg); pos }) ->
    let (env, penv, x) =
      match arg.data with
      | TA_Wildcard ->
        let (env, x) =
          Env.add_anon_tvar ~pos:arg.pos env (T.Kind.fresh_uvar ()) in
        let penv = PartialEnv.add_anon_tvar ~pos PartialEnv.empty x in
        (env, penv, x)

      | TA_Var(xname, kind) ->
        let kind = Type.tr_kind kind in
        let (env, x) = Env.add_tvar ~pos env xname kind in
        let penv = PartialEnv.singleton_tvar ~public ~pos xname x in
        (env, penv, x)
    in
    let tvars = [(np.pos, tr_tname name, x)] in
    (env, penv, tvars, [], T.Pure)

  | NP_Val(NOptionalVar x, pat) ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let sch = BuiltinTypes.mk_option_scheme tp in
    let (penv, pat, eff) = check_scheme env pat sch in
    let arg = (np.pos, Uniqueness.UNOptionalVar x, pat, sch) in
    (env, penv, [], [arg], eff)

  | NP_Val(name, pat) ->
    let (penv, pat, sch, eff) = infer_scheme env pat in
    let arg =
      match name with
      | NVar x -> (np.pos, Uniqueness.UNVar x, pat, sch)
      | NOptionalVar _ -> assert false
      | NImplicit n -> (np.pos, Uniqueness.UNImplicit n, pat, sch)
      | NMethod mname ->
        let owner = TypeUtils.method_owner_of_scheme ~pos:np.pos ~env sch in
        (np.pos, Uniqueness.UNMethod(owner, mname), pat, sch)
    in
    (env, penv, [], [arg], eff)

  | NP_Open | NP_Module _ ->
    Error.fatal (Error.open_pattern_not_allowed ~pos:np.pos)

let infer_named_patterns env named_pats =
  let rec loop env penv named_pats tvars_acc named_acc eff =
    match named_pats with
    | [] ->
      (env, penv, List.rev tvars_acc, List.rev named_acc, eff)
    | np :: named_pats ->
      let (env, penv1, tvars, named, eff1) =
        infer_named_pattern env np in
      let penv = PartialEnv.join ~env penv penv1 in
      let tvars_acc = List.rev_append tvars tvars_acc in
      let named_acc = List.rev_append named named_acc in
      loop env penv named_pats tvars_acc named_acc (T.Effect.join eff eff1)
  in
  let (env, penv, tvars, named, eff) =
    loop env PartialEnv.empty named_pats [] [] T.Pure in
  Uniqueness.check_unif_named_type_args tvars;
  Uniqueness.check_names ~env
    (List.map (fun (pos, name, _, _) -> (pos, name)) named);
  let tvars = List.map (fun (_, name, x) -> (name, x)) tvars in
  let named =
    named |> List.map
      (fun (_, name, x, sch) -> (Uniqueness.tr_name name, x, sch)) in
  (env, penv, tvars, named, eff)

(* ========================================================================= *)

let infer_scheme_ext env (pat : S.pattern) =
  let (penv, pat, sch, eff) = infer_scheme env pat in
  let env = PartialEnv.extend env penv in
  (env, pat, sch, eff)

let check_scheme_ext env (pat : S.pattern) sch =
  let (penv, pat, eff) = check_scheme env pat sch in
  let env = PartialEnv.extend env penv in
  (env, pat, eff)

let check_type_ext env (pat : S.pattern) tp =
  let (penv, pat, eff) = check_type env pat tp in
  let env = PartialEnv.extend env penv in
  (env, pat, eff)

let check_named_patterns_ext
    ~pos env (nps : S.named_pattern list) tvars named =
  let (env, penv, named, eff) =
    check_named_patterns ~pos env PartialEnv.empty nps tvars named in
  let arg_pats = List.filter_map make_arg_pattern named in
  let env = PartialEnv.extend env penv in
  (env, arg_pats, eff)

let infer_named_patterns_ext env (nps : S.named_pattern list) =
  let (env, penv, tvars, named, eff) = infer_named_patterns env nps in
  let scope = Env.scope env in
  let env = PartialEnv.extend env penv in
  (env, scope, tvars, named, eff)
