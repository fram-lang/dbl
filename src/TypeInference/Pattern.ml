(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

open Common

(** Pattern of a named argument *)
type named_pattern = {
          name : Name.t;
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
  List.find_opt (fun np -> Name.equal np.name name) named

let make_pattern ~pos ~pp np =
  let pat =
    match np.pat with
    | None     -> { T.pos; T.pp; T.data = T.PWildcard }
    | Some pat -> pat
  in
  { pat with data = T.PAs(pat, np.var) }

let make_arg_pattern ren np =
  Option.map (fun pat -> (np.var, T.Ren.rename_pattern ren pat)) np.pat

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
let make_module_penv ~pos ~public modname tvars named =
  let types =
    tvars |> List.filter_map
      (fun (name, x) ->
        match name with
        | T.TNAnon     -> None
        | T.TNVar name -> Some (name, x)) in
  let vals =
    named |> List.map
      (fun { name; var; pat = _; sch } -> (name, var, sch)) in
  PartialEnv.singleton_module ~public ~pos ~types ~vals modname

(** Create a partial environment that contains all named parameters. *)
let make_open_penv ~pos ~public ~pp tvars named =
  let add_type penv (name, x) =
    match name with
    | T.TNAnon -> penv
    | T.TNVar name ->
      PartialEnv.add_tvar_alias ~public ~pos penv name x
  in
  let add_value penv { name; var; sch; pat = _ } =
    match name with
    | NVar x | NOptionalVar x ->
      PartialEnv.add_var ~public ~pos ~pp penv x var sch
    | NImplicit n ->
      PartialEnv.add_implicit ~public ~pos ~pp penv n var sch
    | NMethod _ ->
      (* Methods were added to the environment during the opening of the
        constructor. *)
      penv
  in
  let penv = List.fold_left add_type PartialEnv.empty tvars in
  List.fold_left add_value penv named

(* ========================================================================= *)

(** Enter a new scope and introduce all existential types as anonymous type
  variables to the environment and freshly created partial environment.
  Returns both environments, list of refreshed named type variables, and a
  renaming substitution that can be used to refresh the types in the
  constructor. *)
let open_ctor_types ~pos env targs =
  let (env, scope) = Env.enter_scope env in
  let open_ctor_type (env, penv, sub) (tname, x) =
    let (env, y) = Env.add_anon_tvar ~pos env (T.TVar.kind x) in
    let penv = PartialEnv.add_anon_tvar ~pos penv y in
    let sub = T.Subst.rename_tvar sub x y in
    ((env, penv, sub), (tname, y))
  in
  let ((env, penv, sub), tvars) =
    List.fold_left_map open_ctor_type
      (env, PartialEnv.empty, T.Subst.empty ~scope)
      targs
  in
  (env, penv, tvars, sub)

(** Implicitly introduce all named arguments into the environment. Returns
  extended environment and a list of named arguments. *)
let open_ctor_named_args ~pos ~env penv named =
  let pp = Env.pp_tree env in
  let open_named_arg penv (name, sch) =
    let x = Var.fresh () in
    let penv =
      match name with
      | T.NVar _ | T.NOptionalVar _ | T.NImplicit _ -> penv
      | T.NMethod mname ->
        let owner = NameUtils.method_owner_of_scheme ~pos ~pp sch in
        (* Should it be private? *)
        PartialEnv.add_method ~public:false ~pos ~pp penv owner mname x sch
    in
    (penv, (name, x, sch))
  in
  List.fold_left_map open_named_arg penv named

(** Open a constructor: enter a new scope, introduce all existential types as
  anonymous type variables to the environment, create a partial environment
  that contains all implicitly bound variables, and return a list of named
  type variables, initial list of named arguments, and refreshed list of
  arguments' schemes.
  *)
let open_ctor ~pos env (ctor : T.ctor_decl) =
  let (env, penv, tvars, sub) =
    open_ctor_types ~pos env ctor.ctor_targs in
  let named = List.map (T.NamedScheme.subst sub) ctor.ctor_named in
  let (penv, named) = open_ctor_named_args ~pos ~env penv named in
  let arg_schemes = List.map (T.Scheme.subst sub) ctor.ctor_arg_schemes in
  (env, penv, tvars, named, arg_schemes)

(** For a given constructor name and checked type, produce the ADT info for
    the type, the constructor index, proof that it is an ADT, and the needed
    substitution for the type parameters *)
let get_ctor_info ~pos env (cpath : S.ctor_name S.path) tp =
  let pp = Env.pp_tree env in
  match T.Type.whnf tp with
  | Whnf_Neutral(NH_Var x, rev_tps) ->
    begin match ModulePath.lookup_adt env cpath x with
    | Some info ->
      let tps = List.rev rev_tps in
      let sub_add sub (_, tv) tp = T.Subst.add_type sub tv tp in
      let sub = T.Subst.empty ~scope:(Env.scope env) in
      let sub = List.fold_left2 sub_add sub info.adt_args tps in
      let cname = S.path_name cpath in
      begin match T.CtorDecl.find_index info.adt_ctors cname with
      | Some idx ->
        let proof = T.ProofExpr.subst sub info.adt_proof in
        (info, idx, proof, sub)
      | None ->
        Error.fatal (Error.ctor_not_in_type ~pos:cpath.pos ~pp cname tp)
      end
    | None ->
      Error.fatal (Error.ctor_pattern_on_non_adt ~pos ~pp tp)
    end

  | Whnf_Neutral(NH_UVar _, _) ->
    let (idx, info) = ModulePath.lookup_ctor env cpath in
    let (sub, tps) = ParamResolve.guess_types ~pos env info.adt_args in
    let proof = T.ProofExpr.subst sub info.adt_proof in
    (info, idx, proof, sub)

  | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
    Error.fatal (Error.ctor_pattern_on_non_adt ~pos ~pp tp)

  | Whnf_Effect ->
    failwith "Internal kind error"

(* ========================================================================= *)

(** Translate a scheme expression into a target scheme, taking into account
  the special treatment of optional parameter annotations. *)
let tr_named_scheme_annot env (name : Name.t) sch_expr =
  let sch = T.SchemeExpr.to_scheme (Type.tr_scheme env sch_expr) in
  match name with
  | NOptionalVar _ ->
    begin match T.Scheme.to_type sch with
    | Some tp -> BuiltinTypes.mk_option_scheme tp
    | None    ->
      Error.fatal
        (Error.polymorphic_optional_parameter ~pos:sch_expr.sch_pos)
    end
  | NVar _ | NImplicit _ | NMethod _ -> sch

(* ========================================================================= *)

let rec check_scheme env (pat : S.pattern) sch =
  let pos = pat.pos in
  let pp = Env.pp_tree env in
  let make data = T.{ pos; pp; data } in
  match pat.data with
  | PWildcard ->
    (PartialEnv.empty, make T.PWildcard, T.Pure)

  | PId(public, id) ->
    let name = NameUtils.tr_ident ~pos ~pp id sch in
    let x = Var.fresh ~name:(Name.to_string name) () in
    let penv = PartialEnv.singleton_val ~public ~pos name x sch in
    (penv, make (T.PAs(make T.PWildcard, x)), T.Pure)

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
      ~on_error:(Error.pattern_annot_mismatch ~pp sch sch');
    check_scheme env pat sch'

  | POr(pat1, pat2) ->
    let (penv1, pat1, eff1) = check_scheme env pat1 sch in
    let (penv2, pat2, eff2) = check_scheme env pat2 sch in
    let check_schemes sch1 sch2 =
      Error.check_unify_result ~pos
        (Unification.equal_scheme env sch1 sch2)
        ~on_error:(Error.or_pattern_scheme_mismatch ~pp sch1 sch2)
    in
    let scope = Env.scope env in
    let (penv, ren) = PartialEnv.intersect ~check_schemes ~pp ~pos ~scope penv1 penv2 in
    let pat2 = T.Ren.rename_pattern ren pat2 in
    let pat = make (T.POr(pat1, pat2)) in
    let eff = T.Effect.join eff1 eff2 in
    (penv, pat, eff)

and check_type env (pat : S.pattern) tp =
  let pos = pat.pos in
  let pp = Env.pp_tree env in
  let make data = T.{ pos; pp; data } in
  match pat.data with
  | PWildcard | PId _ | PAnnot _ | POr _ ->
    let sch = T.Scheme.of_type tp in
    check_scheme env pat sch

  | PCtor(cpath, named_pats, pats) ->
    let (info, idx, proof, sub) = get_ctor_info ~pos env cpath tp in
    let ctor = T.CtorDecl.subst sub (List.nth info.adt_ctors idx) in
    let res_tp = T.Type.subst sub info.adt_type in
    Error.check_unify_result ~pos (Unification.subtype env tp res_tp)
      ~on_error:(Error.pattern_type_mismatch ~pp res_tp tp);
    let (env, penv, tvars, named, arg_schemes) = open_ctor ~pos env ctor in
    let (env, penv, named, eff1) =
      check_named_patterns ~pos env penv named_pats tvars named in
    let ps1 = List.map (make_pattern ~pos ~pp) named in
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
    let penv = PartialEnv.join ~pp:(Env.pp_tree env) penv penv1 in
    let (penv, pats, eff2) = check_patterns env penv pats schs in
    (penv, pat :: pats, T.Effect.join eff1 eff2)

  | [], _ :: _ | _ :: _, [] -> assert false

and check_named_patterns ~pos env penv named_pats tvars named =
  let prepare_named (name, x, sch) =
    { name = NameUtils.tr_name ~pos ~pp:(Env.pp_tree env) name sch;
      var  = x;
      pat  = None;
      sch  = sch
    } in
  let named = List.map prepare_named named in
  let rec loop env penv named_pats eff =
    match named_pats with
    | [] -> (env, penv, named, eff)
    | np :: named_pats ->
      let (env, penv1, eff1) = check_named_pattern env np tvars named in
      let penv = PartialEnv.join ~pp:(Env.pp_tree env) penv penv1 in
      let eff  = T.Effect.join eff eff1 in
      loop env penv named_pats eff
  in
  loop env penv named_pats T.Pure

and check_named_pattern env np tvars named =
  match np.data with
  | NP_Type(_, { S.data = (TNAnon, _); _ }) ->
    Error.report (Error.anonymous_type_pattern ~pos:np.pos);
    (env, PartialEnv.empty, T.Pure)

  | NP_Type(public, { S.data = (TNVar name, arg); S.pos = pos }) ->
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

  | NP_Val((NVar _ | NOptionalVar _ | NImplicit _) as name, pat, sch_expr) ->
    let pos = np.pos in
    let name =
      match name with
      | NVar x         -> Name.NVar x
      | NOptionalVar x -> Name.NOptionalVar x
      | NImplicit n    -> Name.NImplicit n
      | NMethod _      -> assert false
    in
    begin match select_named_pattern name named with
    | None ->
      let pp = Env.pp_tree env in
      Error.report (Error.unknown_named_pattern ~pos:np.pos ~pp name);
      (env, PartialEnv.empty, T.Pure)

    | Some ({ pat = ppat; sch; _ } as np) ->
      let (penv, pat, eff) =
        match sch_expr with
        | Some sch_expr ->
          let sch' = tr_named_scheme_annot env name sch_expr in
          let pp = Env.pp_tree env in
          Error.check_unify_result ~pos:sch_expr.sch_pos
            (Unification.subscheme env sch sch')
            ~on_error:(Error.pattern_annot_mismatch ~pp sch sch');
          check_scheme env pat sch'
        | None -> check_scheme env pat sch
      in
      begin match ppat with
      | Some ppat ->
        let pp = Env.pp_tree env in
        Error.report
          (Error.multiple_named_patterns ~pos ~ppos:ppat.pos ~pp name)
      | None -> np.pat <- Some pat;
      end;
      (env, penv, eff)
    end

  | NP_Val(NMethod _, _, _) ->
    Error.report (Error.method_pattern_not_allowed ~pos:np.pos);
    (env, PartialEnv.empty, T.Pure)

  | NP_Module(public, modname) ->
    (* Since during type-checking of patterns we use the environment to
      store types, we don't need to add regular variables. *)
    let env = Env.enter_module env in
    let env = add_types_to_env ~pos:np.pos env tvars in
    let env = Env.leave_module env ~public modname in
    let penv =
      make_module_penv ~pos:np.pos ~public modname tvars named in
    (env, penv, T.Pure)

  | NP_Open public ->
    (* Since during type-checking of patterns we use the environment to
      store types, we don't need to add regular variables. *)
    let env = add_types_to_env ~pos:np.pos env tvars in
    let pp = Env.pp_tree env in
    let penv = make_open_penv ~pos:np.pos ~public ~pp tvars named in
    (env, penv, T.Pure)

(* ========================================================================= *)

let infer_scheme env (pat : S.pattern) =
  match pat.data with
  | PWildcard | PId _ | PCtor _ | POr _ ->
    let tp = Env.fresh_uvar ~pos:pat.pos env T.Kind.k_type in
    let tp_expr =
      { T.pos  = pat.pos;
        T.pp   = Env.pp_tree env;
        T.data = T.TE_Type tp
      } in
    let sch = T.SchemeExpr.of_type_expr tp_expr in
    let (penv, pat, eff) = check_type env pat tp in
    (penv, pat, sch, eff)

  | PAnnot(pat, sch) ->
    let sch_expr = Type.tr_scheme env sch in
    let sch = T.SchemeExpr.to_scheme sch_expr in
    let (penv, pat, eff) = check_scheme env pat sch in
    (penv, pat, sch_expr, eff)

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
        (env, PartialEnv.empty, x)

      | TA_Var(xname, kind) ->
        let kind = Type.tr_kind kind in
        let (env, x) = Env.add_tvar ~pos env xname kind in
        let penv = PartialEnv.singleton_tvar_alias ~public ~pos xname x in
        (env, penv, x)
    in
    let tvars = [(np.pos, tr_tname name, x)] in
    (env, penv, tvars, [], T.Pure)

  | NP_Val(NOptionalVar x, pat, sch_expr) ->
    let tp_expr =
      match sch_expr with
      | Some sch_expr ->
        let sch_expr = Type.tr_scheme env sch_expr in
        begin match T.SchemeExpr.to_type_expr sch_expr with
        | Some tp_expr -> tp_expr
        | None    ->
          Error.fatal
            (Error.polymorphic_optional_parameter ~pos:sch_expr.se_pos)
        end
      | None ->
        { T.pos  = np.pos;
          T.pp   = Env.pp_tree env;
          T.data = T.TE_Type (Env.fresh_uvar ~pos:np.pos env T.Kind.k_type)
        }
    in
    let sch_expr = BuiltinTypes.mk_option_scheme_expr tp_expr in
    let (penv, pat, eff) =
      check_scheme env pat (T.SchemeExpr.to_scheme sch_expr) in
    let arg = (np.pos, Name.NOptionalVar x, pat, sch_expr) in
    (env, penv, [], [arg], eff)

  | NP_Val(name, pat, sch_expr) ->
    let (penv, pat, sch_expr, eff) =
      match sch_expr with
      | Some sch_expr ->
        let sch_expr = Type.tr_scheme env sch_expr in
        let (penv, pat, eff) =
          check_scheme env pat (T.SchemeExpr.to_scheme sch_expr) in
        (penv, pat, sch_expr, eff)
      | None ->
        infer_scheme env pat
    in
    let arg =
      match name with
      | NVar x -> (np.pos, Name.NVar x, pat, sch_expr)
      | NOptionalVar _ -> assert false
      | NImplicit n -> (np.pos, Name.NImplicit n, pat, sch_expr)
      | NMethod mname ->
        let pp = Env.pp_tree env in
        let sch = T.SchemeExpr.to_scheme sch_expr in
        let owner = NameUtils.method_owner_of_scheme ~pos:np.pos ~pp sch in
        (np.pos, Name.NMethod(owner, mname), pat, sch_expr)
    in
    (env, penv, [], [arg], eff)

  | NP_Open _ | NP_Module _ ->
    Error.fatal (Error.open_pattern_not_allowed ~pos:np.pos)

let infer_named_patterns env named_pats =
  let (env, _) = Env.enter_scope env in
  let rec loop env penv named_pats tvars_acc named_acc eff =
    match named_pats with
    | [] ->
      (env, penv, List.rev tvars_acc, List.rev named_acc, eff)
    | np :: named_pats ->
      let (env, penv1, tvars, named, eff1) =
        infer_named_pattern env np in
      let pp = Env.pp_tree env in
      let penv = PartialEnv.join ~pp penv penv1 in
      let tvars_acc = List.rev_append tvars tvars_acc in
      let named_acc = List.rev_append named named_acc in
      loop env penv named_pats tvars_acc named_acc (T.Effect.join eff eff1)
  in
  let (env, penv, tvars, named, eff) =
    loop env PartialEnv.empty named_pats [] [] T.Pure in
  Uniqueness.check_unif_named_type_args tvars;
  Uniqueness.check_names ~pp:(Env.pp_tree env)
    (List.map (fun (pos, name, _, _) -> (pos, name)) named);
  let named =
    named |> List.map (fun (_, name, x, sch) -> (name, x, sch)) in
  (env, penv, tvars, named, eff)

(* ========================================================================= *)

let infer_scheme_ext env (pat : S.pattern) =
  let (penv, pat, sch, eff) = infer_scheme env pat in
  let (env, _, _, ren) = PartialEnv.extend env [] penv in
  (env, T.Ren.rename_pattern ren pat, sch, eff)

let check_scheme_ext env (pat : S.pattern) sch =
  let (penv, pat, eff) = check_scheme env pat sch in
  let (env, _, _, ren) = PartialEnv.extend env [] penv in
  (env, T.Ren.rename_pattern ren pat, eff)

let check_type_ext env (pat : S.pattern) tp =
  let (penv, pat, eff) = check_type env pat tp in
  let (env, _, _, ren) = PartialEnv.extend env [] penv in
  (env, T.Ren.rename_pattern ren pat, eff)

let check_named_patterns_ext
    ~pos env (nps : S.named_pattern list) tvars named =
  let (env, penv, named, eff) =
    check_named_patterns ~pos env PartialEnv.empty nps tvars named in
  let (env, _, _, ren) = PartialEnv.extend env [] penv in
  let arg_pats = List.filter_map (make_arg_pattern ren) named in
  (env, arg_pats, eff)

let infer_named_patterns_ext env (nps : S.named_pattern list) =
  let (_, penv, tvars, named, eff) = infer_named_patterns env nps in
  let (env, scope, tvars, ren) = PartialEnv.extend env tvars penv in
  let named = List.map (NameUtils.rename_pattern ren) named in
  (env, scope, tvars, named, eff)
