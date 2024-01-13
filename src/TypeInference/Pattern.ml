(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

let rec check_scheme ~env ~scope (pat : S.pattern) sch =
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

  | PCtor(cname, args) ->
    begin match Env.lookup_ctor env cname.data with
    | Some(idx, info) ->
      let (sub, tps) = ExprUtils.guess_types env info.adt_args in
      let proof  = ExprUtils.make_tapp info.adt_proof tps in
      let ctors  = List.map (T.CtorDecl.subst sub) info.adt_ctors in
      let ctor   = List.nth ctors idx in
      let res_tp = T.Type.subst sub info.adt_type in
      if List.length ctor.ctor_arg_schemes <> List.length args then
        Error.fatal (Error.ctor_arity_mismatch ~pos:pat.pos
          cname.data (List.length ctor.ctor_arg_schemes) (List.length args))
      else if not (Unification.subtype env tp res_tp) then
        Error.fatal (Error.pattern_type_mismatch ~pos:pat.pos ~env
          res_tp tp)
      else
        let (env, ps, _) =
          check_pattern_schemes ~env ~scope args ctor.ctor_arg_schemes in
        let pat = make
          (T.PCtor(cname.data, idx, proof, ctors, ps)) in
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
  | PCtor(_, ps) ->
    List.fold_left (fold_implicit f) acc ps
  | PAnnot(pat, _) ->
    fold_implicit f acc pat
