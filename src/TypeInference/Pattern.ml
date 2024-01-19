(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Select named pattern for given name. On success returns pattern assigned
  to given name and list of remaining named patterns *)
let rec select_inst_pattern name (ips : S.inst_pattern list) =
  match ips with
  | [] -> None
  | { data = IName(n, p); _ } :: ips when n = name -> Some(p, ips)
  | ip :: ips ->
    Option.map
      (fun (p, ips) -> (p, ip :: ips))
      (select_inst_pattern name ips)

let rec check_ctor_type_args ~env ~scope ~sub tvs =
  match tvs with
  | []        -> (env, scope, sub, [])
  | tv :: tvs ->
    let (env, a) = Env.add_anon_tvar env (T.TVar.kind tv) in
    let scope = T.Scope.add scope a in
    let sub   = T.Subst.rename_to_fresh sub tv a in
    let (env, scope, sub, tvs) = check_ctor_type_args ~env ~scope ~sub tvs in
    (env, scope, sub, a :: tvs)

let rec check_ctor_implicit_args ~pos ~env ~scope ips ims =
  match ims with
  | [] ->
    List.iter
      (fun { S.pos; S.data = S.IName(n, _) } ->
        Error.warn (Error.redundant_named_pattern ~pos n))
      ips;
    (env, [])
  | (name, sch) :: ims ->
    begin match select_inst_pattern name ips with
    | None ->
      let (env, x) = Env.add_poly_implicit env name sch ignore in
      let p = { T.pos = pos; T.data = T.PVar(x, sch) } in
      let (env, ps) = check_ctor_implicit_args ~pos ~env ~scope ips ims in
      (env, p :: ps)
    | Some(p, ips) ->
      let (env, p, _) = check_scheme ~env ~scope p sch in
      let (env, ps) = check_ctor_implicit_args ~pos ~env ~scope ips ims in
      (env, p :: ps)
    end

and check_scheme ~env ~scope (pat : S.pattern) sch =
  let make data = { pat with T.data = data } in
  match pat.data with
  | PWildcard ->
    (env, make T.PWildcard, Pure)
  | PVar  x ->
    let (env, x) = Env.add_poly_var env x sch in
    (env, make (T.PVar(x, sch)), Pure)
  | PName n ->
    let (env, x) = Env.add_poly_implicit env n sch ignore in
    (env, make (T.PVar(x, sch)), Pure)
  | PCtor _ ->
    begin match sch with
    | { sch_tvars = []; sch_implicit = []; sch_body = tp } ->
      check_type ~env ~scope pat tp
    | _ ->
      Error.fatal (Error.non_polymorphic_pattern ~pos:pat.pos)
    end
  | PAnnot(pat, sch') ->
    let sch_pos = sch'.sch_pos in
    let sch' = Type.tr_scheme env sch' in
    if not (Unification.subscheme env sch sch') then
      Error.report (Error.pattern_annot_mismatch ~pos:sch_pos ~env sch sch');
    check_scheme ~env ~scope:(Env.scope env) pat sch'

and check_type ~env ~scope (pat : S.pattern) tp =
  let make data = { pat with T.data = data } in
  match pat.data with
  | PWildcard | PVar _ | PName _ | PAnnot _ ->
    let sch = {
      T.sch_tvars    = [];
      T.sch_implicit = [];
      T.sch_body     = tp
    } in
    check_scheme ~env ~scope pat sch

  | PCtor(cname, ips, args) ->
    begin match Env.lookup_ctor env cname.data with
    | Some(idx, info) ->
      let (sub, tps) = ExprUtils.guess_types env info.adt_args in
      let proof  = ExprUtils.make_tapp info.adt_proof tps in
      let ctors  = List.map (T.CtorDecl.subst sub) info.adt_ctors in
      let ctor   = List.nth ctors idx in
      let res_tp = T.Type.subst sub info.adt_type in
      let (env, scope, sub2, tvars) =
        check_ctor_type_args ~env ~scope ~sub:T.Subst.empty ctor.ctor_tvars in
      let ctor_implicit =
        List.map (fun (name, sch) -> (name, T.Scheme.subst sub2 sch))
          ctor.ctor_implicit in
      let ctor_arg_schemes =
        List.map (T.Scheme.subst sub2) ctor.ctor_arg_schemes in
      Uniqueness.check_inst_pattern_uniqueness ips;
      let (env, ps1) =
        check_ctor_implicit_args ~pos:pat.pos ~env ~scope ips ctor_implicit in
      if List.length ctor_arg_schemes <> List.length args then
        Error.fatal (Error.ctor_arity_mismatch ~pos:pat.pos
          cname.data (List.length ctor_arg_schemes) (List.length args))
      else if not (Unification.subtype env tp res_tp) then
        Error.fatal (Error.pattern_type_mismatch ~pos:pat.pos ~env
          res_tp tp)
      else
        let (env, ps2, _) =
          check_pattern_schemes ~env ~scope args ctor_arg_schemes in
        let pat = make
          (T.PCtor(cname.data, idx, proof, ctors, tvars, ps1 @ ps2)) in
        (* Pattern matching is always impure, as due to recursive types it can
          be used to encode non-termination *)
        (env, pat, Impure)

    | None ->
      Error.fatal (Error.unbound_constructor ~pos:cname.pos cname.data)
    end

and check_pattern_schemes ~env ~scope pats schs =
  match pats, schs with
  | [], [] -> (env, [], Pure)

  | pat :: pats, sch :: schs ->
    let (env, pat, r_eff1)  = check_scheme ~env ~scope pat sch in
    let (env, pats, r_eff2) = check_pattern_schemes ~env ~scope pats schs in
    (env, pat :: pats, ret_effect_join r_eff1 r_eff2)

  | [], _ :: _ | _ :: _, [] -> assert false

let infer_arg_scheme env (arg : S.arg) =
  match arg with
  | ArgAnnot(pat, sch) ->
    let sch = Type.tr_scheme env sch in
    let scope = Env.scope env in
    let (env, pat, r_eff) = check_scheme ~env ~scope pat sch in
    (env, pat, sch, r_eff)
  | ArgPattern pat ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let scope = Env.scope env in
    let (env, pat, r_eff) = check_type ~env ~scope pat tp in
    (env, pat, T.Scheme.of_type tp, r_eff)

let check_arg_scheme env (arg : S.arg) sch =
  match arg with
  | ArgAnnot(pat, sch') ->
    let sch_pos = sch'.sch_pos in
    let sch' = Type.tr_scheme env sch' in
    if not (Unification.subscheme env sch sch') then
      Error.report (Error.pattern_annot_mismatch ~pos:sch_pos ~env sch sch');
    check_scheme ~env ~scope:(Env.scope env) pat sch'
  | ArgPattern pat -> check_scheme ~env ~scope:(Env.scope env) pat sch

let infer_inst_arg_scheme env (im : S.inst_arg) =
  match im.data with
  | IName(name, arg) ->
    let (env, pat, sch, r_eff) = infer_arg_scheme env arg in
    (env, (name, pat, sch), r_eff)

let rec infer_inst_arg_schemes env ims =
  match ims with
  | []        -> (env, [], Pure)
  | im :: ims ->
    let (env, im, r_eff1)  = infer_inst_arg_scheme env im in
    let (env, ims, r_eff2) = infer_inst_arg_schemes env ims in
    (env, im :: ims, ret_effect_join r_eff1 r_eff2)

let rec fold_implicit f acc (pat : S.pattern) =
  match pat.data with
  | PWildcard | PVar _ -> acc
  | PName n -> f acc n
  | PCtor(_, ips, ps) ->
    let acc = List.fold_left (fold_implicit_i f) acc ips in
    List.fold_left (fold_implicit f) acc ps
  | PAnnot(pat, _) ->
    fold_implicit f acc pat

and fold_implicit_i f acc (ip : S.inst_pattern) =
  match ip.data with
  | IName(_, p) -> fold_implicit f acc p
