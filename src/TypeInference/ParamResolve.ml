(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving named parameters *)

open Common

module StrMap = Map.Make(String)

(** Reinstantiation information about regular and optional parameters *)
type reinst_regular =
  | RReg of T.var * T.scheme
  | ROpt of T.var * T.typ

(** Reinstantiation contexts. They store information which parameters can be
  reinstantiated. *)
module Reinst : sig
  type t

  (** Empty reinstantiation context *)
  val empty : t

  (** Add a regular parameter to the context *)
  val add_var : t -> S.var -> T.scheme -> t * T.var

  (** Add an optional parameter to the context. The type is a type parameter
    of the [Option] type. *)
  val add_optional : t -> S.var -> T.typ -> t * T.var

  (** Check if a parameter can be reinstantiated *)
  val lookup_var : t -> S.var -> reinst_regular option
end = struct
  type t = reinst_regular StrMap.t

  let empty = StrMap.empty

  let add_var rctx name sch =
    let x = Var.fresh ~name () in
    (StrMap.add name (RReg(x, sch)) rctx, x)

  let add_optional rctx name tp =
    let x = Var.fresh ~name () in
    (StrMap.add name (ROpt(x, tp)) rctx, x)

  let lookup_var rctx name =
    StrMap.find_opt name rctx
end

type reinst_list = Reinst.t

let no_reinst = Reinst.empty

(* ========================================================================= *)
let open_scheme_types ~pos env targs =
  let open_type (env, sub) (tname, x) =
    let name =
      match tname with
      | T.TNAnon  -> None
      | T.TNVar x -> Some x
    in
    let (env, y) = Env.add_anon_tvar ~pos ?name env (T.TVar.kind x) in
    ((env, T.Subst.rename_to_fresh sub x y), (tname, y))
  in
  let ((env, sub), tvs) =
    List.fold_left_map open_type (env, T.Subst.empty) targs in
  (env, tvs, sub)

let open_scheme_values ~pos ~sub env named =
  let open_named (env, rctx) (name, sch) =
    let sch = T.Scheme.subst sub sch in
    match name with
    | T.NVar name ->
      let (rctx, x) = Reinst.add_var rctx name sch in
      ((env, rctx), (x, sch))
    | T.NOptionalVar name ->
      let tp = BuiltinTypes.scheme_to_option_arg sch in
      let (rctx, x) = Reinst.add_optional rctx name tp in
      ((env, rctx), (x, sch))
    | T.NImplicit iname ->
      let (env, x) = Env.add_implicit env iname sch in
      ((env, rctx), (x, sch))
    | T.NMethod name ->
      let pp = Env.pp_tree env in
      let owner = NameUtils.method_owner_of_scheme ~pos ~pp sch in
      let (env, x) = Env.add_method env owner name sch in
      ((env, rctx), (x, sch))
  in
  let ((env, rctx), xs) =
    List.fold_left_map open_named (env, Reinst.empty) named in
  (env, rctx, xs)

let open_scheme ~pos env (sch : T.scheme) =
  let (env, tvs, sub) = open_scheme_types ~pos env sch.sch_targs in
  let (env, rctx, xs) = open_scheme_values ~pos ~sub env sch.sch_named in
  let body = T.Type.subst sub sch.sch_body in
  (env, rctx, List.map snd tvs, xs, body)

let open_scheme_values_explicit ~pos ~sub env named =
  let open_named env (name, sch) =
    let sch = T.Scheme.subst sub sch in
    match name with
    | T.NVar x | T.NOptionalVar x | T.NImplicit x ->
      let x = Var.fresh ~name:x () in
      (env, (name, x, sch))
    | T.NMethod mname ->
      let pp = Env.pp_tree env in
      let owner = NameUtils.method_owner_of_scheme ~pos ~pp sch in
      let (env, x) = Env.add_method env owner mname sch in
      (env, (name, x, sch))
  in
  let (env, named) = List.fold_left_map open_named env named in
  (env, named)

let open_scheme_explicit ~pos env (sch : T.scheme) =
  let (env, tvs, sub) = open_scheme_types ~pos env sch.sch_targs in
  let (env, named) = open_scheme_values_explicit ~pos ~sub env sch.sch_named in
  let body = T.Type.subst sub sch.sch_body in
  (env, tvs, named, body)

(* ========================================================================= *)
(** Create a substitution that replaces type variables with fresh unification
  variables. *)
let guess_types ~pos env targs =
  let guess_type sub (_, x) =
    let tp = Env.fresh_uvar env (T.TVar.kind x) in
    (T.Subst.add_type sub x tp, { T.pos; T.data = T.TE_Type tp })
  in
  List.fold_left_map guess_type T.Subst.empty targs

(** Check for cyclic dependencies in the type parameters, and extend a set of
  restricted variables. *)
let restrict_var ~vset ~pos ~pp x name =
  if Var.Set.mem x vset then
    Error.fatal (Error.looping_named_param ~pos ~pp name)
  else
    Var.Set.add x vset

(* ------------------------------------------------------------------------- *)

let rec coerce_scheme ~vset ~pos ~name env e (sch_in : T.scheme) sch_out =
  let (env, rctx, tvs, xs, tp_out) = open_scheme ~pos env sch_out in
  (* Now, we do basically the same as in [instantiate], but we perform
    unification just afeter guessing types, in order to get more precise
    types in resolving of named parameters. *)
  let (sub, tps) = guess_types ~pos env sch_in.sch_targs in
  let tp_in = T.Type.subst sub sch_in.sch_body in
  let pp = Env.pp_tree env in
  Error.check_unify_result ~pos
    (Unification.subtype env tp_in tp_out)
    ~on_error:(Error.named_param_type_mismatch ~pp name tp_in tp_out);
  let (named, cs) =
    resolve_params ~sub ~vset ~pos env rctx sch_in.sch_named in
  let make data = { T.pos; T.data } in
  let e = make (T.EPolyFun(tvs, xs, make (T.EInst(e, tps, named)))) in
  (e, cs)

and resolve_params ~sub ~vset ~pos env rctx named =
  let resolve cs1 (name, sch) =
    let sch  = T.Scheme.subst sub sch in
    let (e, cs2) = resolve_param ~vset ~pos env rctx name sch in
    (cs1 @ cs2, e)
  in
  let (cs, args) = List.fold_left_map resolve [] named in
  (args, cs)

and resolve_param ~vset ~pos env rctx name sch =
  match name with
  | T.NVar x ->
    begin match Reinst.lookup_var rctx x with
    | Some (RReg(y, y_sch)) ->
      let vset = restrict_var ~vset ~pos ~pp:(Env.pp_tree env) y (NVar x) in
      let e = { T.pos; T.data = T.EVar y } in
      coerce_scheme ~vset ~pos ~name:(NVar x) env e y_sch sch
    | Some (ROpt _) | None ->
      Error.fatal (Error.cannot_resolve_named_param ~pos x)
    end

  | T.NOptionalVar x ->
    resolve_optional ~vset ~pos env rctx x sch

  | T.NImplicit iname ->
    resolve_implicit ~vset ~pos env rctx iname sch

  | T.NMethod mname ->
    resolve_method ~vset ~pos env rctx mname sch

and resolve_optional ~vset ~pos env rctx x sch =
  let tp = BuiltinTypes.scheme_to_option_arg sch in
  let name = Name.NOptionalVar x in
  let pp = Env.pp_tree env in
  begin match Reinst.lookup_var rctx x with
  | Some (ROpt(y, y_tp)) ->
    Error.check_unify_result ~pos
      (Unification.subtype env y_tp tp)
      ~on_error:(Error.named_param_type_mismatch ~pp name y_tp tp);
    let e = { T.pos; T.data = T.EVar y } in
    (e, [])

  | Some (RReg(y, y_sch)) ->
    let vset = restrict_var ~vset ~pos ~pp y name in
    let e = { T.pos; T.data = T.EVar y } in
    let (e, cs) =
      coerce_scheme ~vset ~pos ~name env e y_sch (T.Scheme.of_type tp) in
    let e = BuiltinTypes.mk_some_poly ~pos tp e in
    let e = { T.pos; T.data = T.EPolyFun([], [], e) } in
    (e, cs)

  | None ->
    let e = BuiltinTypes.mk_none ~pos tp in
    let e = { T.pos; T.data = T.EPolyFun([], [], e) } in
    (e, [])
  end

and resolve_implicit ~vset ~pos env rctx iname sch =
  let name = Name.NImplicit iname in
  match ModulePath.try_lookup_implicit ~pos env iname with
  | None ->
    Error.fatal (Error.cannot_resolve_implicit ~pos iname)
  | Some(x, x_sch) ->
    let vset = restrict_var ~vset ~pos ~pp:(Env.pp_tree env) x name in
    let e = { T.pos; T.data = T.EVar x } in
    coerce_scheme ~vset ~pos ~name env e x_sch sch

and resolve_method ~vset ~pos env rctx mname (sch : T.scheme) =
  let self_tp =
    match T.Type.view sch.sch_body with
    | TArrow(owner_sch, _, _) ->
      assert (T.Scheme.is_monomorphic owner_sch);
      owner_sch.sch_body
    | _ -> assert false
  in
  let pp = Env.pp_tree env in
  match NameUtils.method_owner_of_self self_tp with
  | Some owner ->
    let name = Name.NMethod(owner, mname) in
    begin match ModulePath.try_lookup_method ~pos env owner mname with
    | Some(x, x_sch) ->
      let vset = restrict_var ~vset ~pos ~pp x name in
      let e = { T.pos; T.data = T.EVar x } in
      coerce_scheme ~vset ~pos ~name env e x_sch sch

    | None ->
      Error.fatal (Error.cannot_resolve_method ~pos ~pp owner mname)
    end
  | None ->
    (* TODO: create a method constraint *)
    failwith "Not implemented: method call on unknown type"

(* ========================================================================= *)
let instantiate ~pos env rctx poly_expr (sch : T.scheme) =
  let vset = Var.Set.empty in
  let (sub, tps) = guess_types ~pos env sch.sch_targs in
  let (named, cs) = resolve_params ~sub ~vset ~pos env rctx sch.sch_named in
  let e = { T.pos; T.data = T.EInst(poly_expr, tps, named) } in
  (e, T.Type.subst sub sch.sch_body, cs)

let coerce_scheme ~pos ~name env poly_expr sch_in sch_out =
  let vset = Var.Set.empty in
  coerce_scheme ~vset ~pos ~name env poly_expr sch_in sch_out

let resolve_implicit ~pos env iname sch =
  let vset = Var.Set.empty in
  resolve_implicit ~vset ~pos env no_reinst iname sch

let resolve_method ~pos env mname sch =
  let vset = Var.Set.empty in
  resolve_method ~vset ~pos env no_reinst mname sch
