(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

open Common

type named_type    = (T.tname * S.type_arg) S.node
type named_pattern = (T.name * S.pattern) S.node

let tr_named_type (ntp : S.named_type_arg) =
  let (name, arg) = ntp.data in
  { ntp with data = (Name.tr_tname name, arg) }

let tr_named_pattern env (np : S.named_pattern) =
  let (name, pat) = np.data in
  { np with data = (Name.tr_name env name, pat) }

let union_bound_names bn1 bn2 =
  T.Name.Map.merge
    (fun name p1 p2 ->
      match p1, p2 with
      | None, _ -> p2
      | _, None -> p1
      | Some pos1, Some pos2 ->
        Error.report (Error.multiple_name_binders ~pos1 ~pos2 name);
        Some pos1)
    bn1 bn2

(** Select named type for given name. On success returns type argument assigned
  to given name and list of remaining named types *)
let rec select_named_type name (ntps : named_type list) =
  match ntps with
  | [] -> None
  | { data = (n, a); _ } :: ntps when n = name -> Some(a, ntps)
  | ntp :: ntps ->
    Option.map
      (fun (a, npts) -> (a, ntp :: ntps))
      (select_named_type name ntps)

(** Select named pattern for given name. On success returns pattern assigned
  to given name and list of remaining named patterns *)
let rec select_named_pattern name (nps : named_pattern list) =
  match nps with
  | [] -> None
  | { data = (n, p); _ } :: nps when T.Name.equal n name -> Some(p, nps)
  | np :: nps ->
    Option.map
      (fun (p, nps) -> (p, np :: nps))
      (select_named_pattern name nps)

let rec check_ctor_type_args ~pos ~env ~scope ~sub
    (ntps : named_type list) (tvs : T.named_tvar list) =
  match tvs with
  | []        ->
    List.iter
      (fun {S.pos; S.data = (n, _) } ->
        Error.warn (Error.redundant_named_type ~pos n))
      ntps;
    (env, scope, sub, [])
  | (name, tv) :: tvs ->
    let (env, a, ntps) =
      match select_named_type name ntps with
      | None ->
        let (env, a) = Env.add_anon_tvar ~pos env (T.TVar.kind tv) in
        (env, a, ntps)
      | Some(arg, ntps) ->
        let (env, a) = Type.check_type_arg env arg (T.TVar.kind tv) in
        (env, a, ntps)
    in
    let scope = T.Scope.add scope a in
    let sub   = T.Subst.rename_to_fresh sub tv a in
    let (env, scope, sub, tvs) =
      check_ctor_type_args ~pos ~env ~scope ~sub ntps tvs in
    (env, scope, sub, a :: tvs)

(** Extend the environment by a named parameter that is not explicitly
  mentioned. The middle element of returned triple is a set of names
  implicity bound. *)
let introduce_implicit_name ~pos env (name : T.name) sch =
  match name with
  | NLabel ->
    (* Do not introduce anything. Just create a fresh variable *)
    (env, T.Name.Map.empty, Var.fresh ~name:"label" ())

  | NVar x | NOptionalVar x ->
    (* Do not introduce anything. Just create a fresh variable *)
    (env, T.Name.Map.empty, Var.fresh ~name:x ())

  | NImplicit n ->
    (* Implicit parameters are implicitly introduced to the environment.
     This behavior might be considered controversial. We will see how it works
     in practice. *)
    let (env, x) = Env.add_poly_implicit env n sch ignore in
    (env, T.Name.Map.singleton name pos, x)

    (* Methods are implicitly introduced to the environment.*)
  | NMethod n ->
    let owner = TypeUtils.method_owner_of_scheme ~pos:pos ~env:env sch in
    let (env, x) = Env.add_poly_method env owner n sch in
    (env, T.Name.Map.singleton name pos, x)
    
(** For a given constructor name and checked type, produce the ADT info for
    the type, the constructor index, proof that it is an ADT, and the needed
    substitution for the type parameters *)
let get_ctor_info ~pos ~env (cpath : S.ctor_name S.path S.node) tp =
  match T.Type.whnf tp with
  | Whnf_Neutral(NH_Var x, rev_tps) ->
    begin match Env.lookup_adt env x with
    | Some info ->
      let tps = List.rev rev_tps in
      let sub_add sub (_, tv) tp = T.Subst.add_type sub tv tp in
      let sub = List.fold_left2 sub_add T.Subst.empty info.adt_args tps in
      let cname = S.path_name cpath.data in
      begin match T.CtorDecl.find_index info.adt_ctors cname with
      | Some idx ->
        let proof = ExprUtils.make_tapp info.adt_proof tps in
        (info, idx, proof, sub)
      | None ->
        Error.fatal (Error.ctor_not_in_type ~pos:cpath.pos ~env cname tp)
      end
    | None ->
      Error.fatal (Error.ctor_pattern_on_non_adt ~pos ~env tp)
    end

  | Whnf_Neutral(NH_UVar _, _) ->
    begin match Env.lookup_ctor env cpath.data with
    | Some(idx, info) ->
      let (sub, tps) = ExprUtils.guess_types ~pos env info.adt_args in
      let proof = ExprUtils.make_tapp info.adt_proof tps in
      (info, idx, proof, sub)
    | None ->
      Error.fatal (Error.unbound_constructor ~pos:cpath.pos cpath.data)
    end

  | Whnf_PureArrow _ | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
    Error.fatal (Error.ctor_pattern_on_non_adt ~pos ~env tp)

  | Whnf_Effect _ | Whnf_Effrow _ ->
    failwith "Internal kind error"

(** Introduce all named types (from the constructor scheme) into the
  environment. Returns extended environment, refreshed type parameters, and a
  renaming used for refreshing. *)
let open_named_types ~public env targs =
  let open_named_type (env, sub) (name, x) =
    let (env, y) =
      let kind = T.TVar.kind x in
      match name with
      | T.TNAnon   -> Env.add_anon_tvar env kind
      | T.TNEffect ->
        assert (T.Kind.is_effect kind);
        Env.add_the_effect env
      | T.TNVar x  -> Env.add_tvar ~public env x kind
    in
    ((env, T.Subst.rename_to_fresh sub x y), y)
  in
  let ((env, sub), targs) =
    List.fold_left_map open_named_type (env, T.Subst.empty) targs in
  (env, targs, sub)

(** Introduce single named argument into the environment. Its scheme is
  refreshed using given substitution. *)
let open_named_arg ~pos ~public ~sub env (name, sch) =
  let sch = T.Scheme.subst sub sch in
  let (env, x) =
    match name with
    | T.NLabel -> Env.add_the_label_sch env sch
    | T.NVar x -> Env.add_poly_var ~public env x sch
    | T.NOptionalVar x -> Env.add_mono_var ~public env x sch.sch_body
    | T.NImplicit n -> Env.add_poly_implicit ~public env n sch ignore
    | T.NMethod   n ->
      let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
      Env.add_poly_method ~public env owner n sch
  in
  (env, T.{ pos; data = PVar(x, sch) })

(** Same as [open_named_arg], but works on list of named parameters. *)
let open_named_args ~pos ~public ~sub env named =
  List.fold_left_map (open_named_arg ~pos ~public ~sub) env named

(** Introduce all named types and named arguments into the environment.
  The type variables are refreshed, so the function returns a renaming. *)
let open_named ~pos ~public env targs named =
  let (env, targs, sub) = open_named_types ~public env targs in
  let (env, named) = open_named_args ~pos ~public ~sub env named in
  (env, targs, named, sub)

let rec check_ctor_named ~pos ~env ~scope
    (cpath : S.ctor_name S.path S.node) (ctor : T.ctor_decl) named =
  match named with
  | S.CNParams(targs, nps) ->
    Uniqueness.check_named_type_arg_uniqueness targs;
    let targs = List.map tr_named_type targs in
    let (env, scope, sub2, tvars) =
      check_ctor_type_args ~pos:cpath.pos ~env ~scope ~sub:T.Subst.empty
        targs ctor.ctor_targs in
    let ctor_named = List.map (T.NamedScheme.subst sub2) ctor.ctor_named in
    Uniqueness.check_named_pattern_uniqueness nps;
    let nps = List.map (tr_named_pattern env) nps in
    let (env, bn1, ps1) =
      check_ctor_named_args ~pos ~env ~scope nps ctor_named in
    (env, scope, sub2, tvars, ps1, bn1)
  | S.CNModule modname ->
    (* TODO: This may seem inconsistent with the other case as implicits aren't
       introduced to the current namespace, just the provided module name. *)
    let env = Env.enter_module env in
    let (env, tvars, ps1, sub2) =
      open_named ~pos ~public:true env ctor.ctor_targs ctor.ctor_named in
    let env = Env.leave_module env ~public:false modname in
    (env, Env.scope env, sub2, tvars, ps1, T.Name.Map.empty)

and check_ctor_named_args ~pos ~env ~scope nps named =
  match named with
  | [] ->
    List.iter
      (fun { S.pos; S.data = (n, _) } ->
        Error.warn (Error.redundant_named_pattern ~pos n))
      nps;
    (env, T.Name.Map.empty, [])
  | (name, sch) :: named ->
    begin match select_named_pattern name nps with
    | None ->
      let (env, bn1, x) = introduce_implicit_name ~pos env name sch in
      let p = { T.pos = pos; T.data = T.PVar(x, sch) } in
      let (env, bn2, ps) =
        check_ctor_named_args ~pos ~env ~scope nps named in
      let bn = union_bound_names bn1 bn2 in
      (env, bn, p :: ps)
    | Some(p, nps) ->
      let (env, p, bn1, _) = check_scheme ~env ~scope p sch in
      let (env, bn2, ps) =
        check_ctor_named_args ~pos ~env ~scope nps named in
      let bn = union_bound_names bn1 bn2 in
      (env, bn, p :: ps)
    end

and check_scheme ~env ~scope (pat : S.pattern) sch =
  let make data = { pat with T.data = data } in
  match pat.data with
  | PWildcard ->
    (env, make T.PWildcard, T.Name.Map.empty, Pure)
  | PId (IdVar(public, x)) ->
    let bn = T.Name.Map.singleton (T.NVar x) pat.pos in
    let (env, x) = Env.add_poly_var env ~public x sch in
    (env, make (T.PVar(x, sch)), bn, Pure)
  | PId (IdImplicit(public, n)) ->
    let bn = T.Name.Map.singleton (T.NImplicit n) pat.pos in
    let (env, x) = Env.add_poly_implicit env ~public n sch ignore in
    (env, make (T.PVar(x, sch)), bn, Pure)
  | PId (IdMethod(public, name)) ->
    (* TODO: bettern bn *)
    let bn = T.Name.Map.empty in
    let owner = TypeUtils.method_owner_of_scheme ~pos:pat.pos ~env sch in
    let (env, x) = Env.add_poly_method env ~public owner name sch in
    (env, make (T.PVar(x, sch)), bn, Pure)
  | PCtor _ | PId IdLabel ->
    begin match sch with
    | { sch_targs = []; sch_named = []; sch_body = tp } ->
      check_type ~env ~scope pat tp
    | _ ->
      Error.fatal (Error.non_polymorphic_pattern ~pos:pat.pos)
    end
  | PAnnot(pat, sch') ->
    let sch_pos = sch'.sch_pos in
    let sch' = Type.tr_scheme env sch' in
    Error.check_unify_result ~pos:sch_pos
      (Unification.subscheme env sch sch')
      ~on_error:(Error.pattern_annot_mismatch ~env sch sch');
    check_scheme ~env ~scope:(Env.scope env) pat sch'

and check_type ~env ~scope (pat : S.pattern) tp =
  let make data = { pat with T.data = data } in
  let pos = pat.pos in
  match pat.data with
  | PWildcard | PId (IdVar _ | IdImplicit _ | IdMethod _) | PAnnot _ ->
    let sch = T.Scheme.of_type tp in
    check_scheme ~env ~scope pat sch

  | PId IdLabel ->
    begin match Unification.as_label env tp with
    | L_Label(eff, tp0, eff0) ->
      let bn = T.Name.Map.singleton T.NLabel pat.pos in
      let (env, x) = Env.add_the_label env eff tp0 eff0 in
      (env, make (T.PVar(x, T.Scheme.of_type tp)), bn, Pure)

    | L_NoEffect ->
      Error.fatal (Error.cannot_guess_label_effect ~pos)

    | L_No ->
      Error.fatal
        (Error.label_pattern_type_mismatch ~pos:pat.pos ~env tp)
    end

  | PCtor(cpath, named, args) ->
    let (info, idx, proof, sub) = get_ctor_info ~pos ~env cpath tp in
    let ctors  = List.map (T.CtorDecl.subst sub) info.adt_ctors in
    let ctor   = List.nth ctors idx in
    let res_tp = T.Type.subst sub info.adt_type in
    let (env, scope, sub2, tvars, ps1, bn1) =
      check_ctor_named ~pos ~env ~scope cpath ctor named in
    let ctor_arg_schemes =
      List.map (T.Scheme.subst sub2) ctor.ctor_arg_schemes in
    if List.length ctor_arg_schemes <> List.length args then
      Error.fatal (Error.ctor_arity_mismatch ~pos:pat.pos
        cpath.data (List.length ctor_arg_schemes) (List.length args))
    else
      Error.check_unify_result ~is_fatal:true ~pos:pat.pos
        (Unification.subtype env tp res_tp)
        ~on_error:(Error.pattern_type_mismatch ~env res_tp tp);
      let (env, ps2, bn2, _) =
        check_pattern_schemes ~env ~scope args ctor_arg_schemes in
      let cname = S.path_name cpath.data in
      let pat = make
        (T.PCtor(cname, idx, proof, ctors, tvars, ps1 @ ps2)) in
      (* Pattern matching is always impure, as due to recursive types it can
        be used to encode non-termination *)
      (env, pat, union_bound_names bn1 bn2, Impure)

and check_pattern_schemes ~env ~scope pats schs =
  match pats, schs with
  | [], [] -> (env, [], T.Name.Map.empty, Pure)

  | pat :: pats, sch :: schs ->
    let (env, pat, bn1, r_eff1)  = check_scheme ~env ~scope pat sch in
    let (env, pats, bn2, r_eff2) =
      check_pattern_schemes ~env ~scope pats schs in
    let bn = union_bound_names bn1 bn2 in
    (env, pat :: pats, bn, ret_effect_join r_eff1 r_eff2)

  | [], _ :: _ | _ :: _, [] -> assert false

let infer_optional_arg_scheme env (arg : S.arg) =
match arg with
| ArgAnnot(pat, sch) ->
  let sch_pos = sch.sch_pos in
  let sch = Type.tr_scheme env sch in
  assert (T.Scheme.is_monomorphic sch);
  let sch = T.Scheme.of_type 
    (PreludeTypes.mk_Option ~env ~pos:sch_pos sch.sch_body)
  in
  let scope = Env.scope env in
  let (env, pat, _, r_eff) =
    check_scheme ~env ~scope pat sch in
  (env, pat, sch, r_eff)
| ArgPattern pat ->
  let tp = Env.fresh_uvar env T.Kind.k_type in
  let tp = (PreludeTypes.mk_Option ~env ~pos:pat.pos tp) in
  let scope = Env.scope env in
  let (env, pat, _, r_eff) = check_type ~env ~scope pat tp in
  (env, pat, T.Scheme.of_type tp, r_eff)

let infer_arg_scheme env (arg : S.arg) =
  match arg with
  | ArgAnnot(pat, sch) ->
    let sch = Type.tr_scheme env sch in
    let scope = Env.scope env in
    let (env, pat, _, r_eff) =
      check_scheme ~env ~scope pat sch in
    (env, pat, sch, r_eff)
  | ArgPattern pat ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let scope = Env.scope env in
    let (env, pat, _, r_eff) = check_type ~env ~scope pat tp in
    (env, pat, T.Scheme.of_type tp, r_eff)

let check_arg_scheme env (arg : S.arg) sch =
  match arg with
  | ArgAnnot(pat, sch') ->
    let sch_pos = sch'.sch_pos in
    let sch' = Type.tr_scheme env sch' in
    Error.check_unify_result ~pos:sch_pos
      (Unification.subscheme env sch sch')
      ~on_error:(Error.pattern_annot_mismatch ~env sch sch');
    let (env, pat, _, r_eff) =
      check_scheme ~env ~scope:(Env.scope env) pat sch' in
    (env, pat, r_eff)
  | ArgPattern pat ->
    let (env, pat, _, r_eff) =
      check_scheme ~env ~scope:(Env.scope env) pat sch in
    (env, pat, r_eff)

let infer_named_arg_scheme env (na : S.named_arg) =
  let (name, arg) = na.data in
  (* Pattern match on name, act accordingly if it's Optional Arg *)
  let name = Name.tr_name env name in
  let (env, pat, sch, r_eff) = 
    begin match name with
    | NOptionalVar x -> infer_optional_arg_scheme env arg
    | _ -> infer_arg_scheme env arg
    end
  in
  begin match name with
  | NLabel ->
    let { T.sch_targs; sch_named; sch_body } = sch in
    if not (List.is_empty sch_targs && List.is_empty sch_named) then
      Error.fatal (Error.polymorphic_label ~pos:na.pos);
    begin match Unification.as_label env sch_body with
    | L_Label _ -> ()
    | L_NoEffect ->
      Error.fatal (Error.cannot_guess_label_effect ~pos:na.pos)
    | L_No ->
      Error.fatal (Error.label_type_mismatch ~pos:na.pos)
    end
  | NVar _ | NOptionalVar _ | NImplicit _ | NMethod _ -> ()
  end;
  (env, (name, pat, sch), r_eff)

let rec infer_named_arg_schemes env ims =
  match ims with
  | []        -> (env, [], Pure)
  | im :: ims ->
    let (env, im, r_eff1)  = infer_named_arg_scheme env im in
    let (env, ims, r_eff2) = infer_named_arg_schemes env ims in
    (env, im :: ims, ret_effect_join r_eff1 r_eff2)
