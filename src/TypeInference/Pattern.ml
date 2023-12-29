(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

let rec check_scheme ~env ~scope (pat : S.pattern) sch =
  let make data = { pat with T.data = data } in
  match pat.data with
  | PWildcard ->
    (env, make T.PWildcard)
  | PVar  x ->
    let (env, x) = Env.add_poly_var env x sch in
    (env, make (T.PVar(x, sch)))
  | PName n ->
    let (env, x) = Env.add_poly_implicit env n sch ignore in
    (env, make (T.PVar(x, sch)))
  | PCtor _ ->
    begin match sch with
    | { sch_tvars = []; sch_implicit = []; sch_body = tp } ->
      check_type ~env ~scope pat tp
    | _ ->
      Error.fatal (Error.non_polymorphic_pattern ~pos:pat.pos)
    end

and check_type ~env ~scope (pat : S.pattern) tp =
  let make data = { pat with T.data = data } in
  match pat.data with
  | PWildcard | PVar _ | PName _ ->
    let sch = {
      T.sch_tvars    = [];
      T.sch_implicit = [];
      T.sch_body     = tp
    } in
    check_scheme ~env ~scope (pat : S.pattern) sch

  | PCtor(cname, args) ->
    begin match Env.lookup_ctor env cname.data with
    | Some(idx, info) ->
      let ctor = List.nth info.adt_ctors idx in
      if List.length ctor.ctor_arg_types <> List.length args then
        Error.fatal (Error.ctor_arity_mismatch ~pos:pat.pos
          cname.data (List.length ctor.ctor_arg_types) (List.length args))
      else if not (Unification.subtype env tp info.adt_type) then
        Error.fatal (Error.pattern_type_mismatch ~pos:pat.pos ~env
          info.adt_type tp)
      else
        let (env, ps) =
          check_pattern_types ~env ~scope args ctor.ctor_arg_types in
        (env, make
          (T.PCtor(cname.data, idx, info.adt_proof, info.adt_ctors, ps)))

    | None ->
      Error.fatal (Error.unbound_constructor ~pos:cname.pos cname.data)
    end

and check_pattern_types ~env ~scope pats tps =
  match pats, tps with
  | [], [] -> (env, [])

  | pat :: pats, tp :: tps ->
    let (env, pat)  = check_type ~env ~scope pat tp in
    let (env, pats) = check_pattern_types ~env ~scope pats tps in
    (env, pat :: pats)

  | [], _ :: _ | _ :: _, [] -> assert false
