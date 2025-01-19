(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kind-checking and translation of type expressions *)

open Common

module StrSet = Set.Make(String)

let rec tr_kind (k : S.kind_expr) =
  match k.data with
  | KWildcard      -> T.Kind.fresh_uvar ()
  | KType          -> T.Kind.k_type
  | KEffect        -> T.Kind.k_effect
  | KArrow(k1, k2) -> T.Kind.k_arrow (tr_kind k1) (tr_kind k2)

let rec infer_kind env (tp : S.type_expr) =
  let pos = tp.pos in
  let make data = { tp with data } in
  match tp.data with
  | TWildcard ->
    let k = T.Kind.fresh_uvar () in
    (make (T.TE_Type(Env.fresh_uvar env k)), k)
  
  | TVar path ->
    let tp = ModulePath.lookup_type env path in
    (make (T.TE_Type tp), T.Type.kind tp)

  | TEffect effs ->
    let effs = List.map (tr_effect env) effs in
    (make (T.TE_Effect effs), T.Kind.k_effect)

  | TPureArrow(sch, tp) ->
    let sch = tr_scheme env sch in
    let tp  = tr_ttype  env tp  in
    (make (T.TE_PureArrow(sch, tp)), T.Kind.k_type)

  | TArrow(sch, tp, eff) ->
    let sch = tr_scheme env sch in
    let tp  = tr_ttype  env tp  in
    let eff = tr_effect env eff in
    (make (T.TE_Arrow(sch, tp, eff)), T.Kind.k_type)

  | THandler { effect; cap_type; in_type; in_eff; out_type; out_eff } ->
    let (in_env, effect) = Env.add_tvar ~pos env effect T.Kind.k_effect in
    let cap_type = tr_ttype  in_env cap_type in
    let in_type  = tr_ttype  in_env in_type in
    let in_eff   = tr_effect in_env in_eff in
    let out_type = tr_ttype  env out_type in
    let out_eff  = tr_effect env out_eff in
    let tp = make (T.TE_Handler
      { effect; cap_type; in_type; in_eff; out_type; out_eff }) in
    (tp, T.Kind.k_type)

  | TLabel { effect; delim_tp; delim_eff } ->
    let effect    = tr_effect env effect in
    let delim_tp  = tr_ttype  env delim_tp in
    let delim_eff = tr_effect env delim_eff in
    let tp = make (T.TE_Label { effect; delim_tp; delim_eff }) in
    (tp, T.Kind.k_type)

  | TApp(tp1, tp2) ->
    let pos1 = tp1.pos in
    let (tp1, k1) = infer_kind env tp1 in
    begin match Unification.kind_to_arrow k1 with
    | Some(k2, kv) ->
      let tp2 = check_kind env tp2 k2 in
      (make (T.TE_App(tp1, tp2)), kv)
    | None ->
      Error.fatal (Error.type_not_function ~pos:pos1 k1)
    end
    
and check_kind env (tp : S.type_expr) k =
  let (tp, k') = infer_kind env tp in
  if Unification.unify_kind k k' then tp
  else
    Error.fatal (Error.kind_mismatch ~pos:tp.pos k' k);

and tr_scheme env (sch : S.scheme_expr) =
  let (env, targs, named) = tr_scheme_args env sch.sch_args in
  { T.se_pos = sch.sch_pos;
    T.se_targs = targs;
    T.se_named = named;
    T.se_body  = tr_ttype env sch.sch_body
  }

and tr_scheme_args ?(data_targs=[]) env args =
 let rec loop env targs named args =
    match args with
    | [] -> (env, List.rev targs, List.rev named)
    | arg :: args ->
      let (env, targs1, named1) = tr_scheme_arg ~data_targs env arg in
      let targs = List.rev_append targs1 targs in
      let named = List.rev_append named1 named in
      loop env targs named args
  in
  let (env, targs, named) = loop env [] [] args in
  Uniqueness.check_unif_named_type_args targs;
  Uniqueness.check_names ~pp:(Env.pp_tree env)
    (List.map (fun (pos, name, _) -> (pos, name)) named);
  let targs = List.map (fun (_, name, x) -> (name, x)) targs in
  let named =
    List.map
      (fun (_, name, sch) -> (Name.to_unif name, sch))
      named in
  (env, targs, named)

and tr_scheme_arg ~data_targs env (arg : S.scheme_arg) =
  let pos = arg.pos in
  match arg.data with
  | SA_Type(S.TNAnon, x, kind) ->
    let kind = tr_kind kind in
    let (env, x) = Env.add_tvar ~pos env x kind in
    (env, [(pos, T.TNAnon, x)], [])

  | SA_Type(S.TNVar name, x, kind) ->
    if List.mem_assoc (T.TNVar name) data_targs then
      Error.report (Error.ctor_type_arg_same_as_data_arg ~pos name);
    let kind = tr_kind kind in
    let (env, x) = Env.add_tvar ~pos env x kind in
    (env, [(pos, T.TNVar name, x)], [])

  | SA_Val(S.NVar name, sch) ->
    let sch = tr_scheme env sch in
    (env, [], [(pos, Name.NVar name, sch)])

  | SA_Val(S.NOptionalVar name, sch) ->
    let sch = tr_scheme env sch in
    begin match T.SchemeExpr.to_type_expr sch with
    | Some tp ->
      let sch = BuiltinTypes.mk_option_scheme_expr tp in
      (env, [], [(pos, Name.NOptionalVar name, sch)])
    | None ->
      Error.fatal (Error.polymorphic_optional_parameter ~pos)
    end

  | SA_Val(S.NImplicit name, sch) ->
    let sch = tr_scheme env sch in
    (env, [], [(pos, Name.NImplicit name, sch)])

  | SA_Val(S.NMethod name, sch) ->
    let sch = tr_scheme env sch in
    let owner =
      NameUtils.method_owner_of_scheme ~pos ~pp:(Env.pp_tree env)
        (T.SchemeExpr.to_scheme sch) in
    (env, [], [(pos, Name.NMethod(owner, name), sch)])

and tr_ttype env tp =
  check_kind env tp T.Kind.k_type

and tr_effect env tp =
  check_kind env tp T.Kind.k_effect

let check_type_arg env (arg : S.type_arg) kind =
  let pos = arg.pos in
  match arg.data with
  | TA_Var(name, k) ->
    let kind_annot = tr_kind k in
    if not (Unification.unify_kind kind kind_annot) then
      Error.fatal (Error.kind_annot_mismatch ~pos kind kind_annot);
    let (env, x) = Env.add_tvar ~pos env name kind in
    (env, T.TNVar name, x)

  | TA_Wildcard ->
    let (env, x) = Env.add_anon_tvar ~pos env kind in
    (env, T.TNAnon, x)

let tr_named_type_arg env (arg : S.named_type_arg) =
  let pos = arg.pos in
  let (name, arg) = arg.data in
  let name = tr_tname name in
  match arg.data with
  | TA_Var(x, k) ->
    let kind = tr_kind k in
    let (env, x) = Env.add_tvar ~pos:arg.pos env x kind in
    (env, (pos, name, x))

  | TA_Wildcard ->
    let kind = T.Kind.fresh_uvar () in
    let (env, x) = Env.add_anon_tvar ~pos env kind in
    (env, (pos, name, x))

let tr_named_type_args env args =
  let (env, args) = List.fold_left_map tr_named_type_arg env args in
  Uniqueness.check_unif_named_type_args args;
  let args = List.map (fun (_, name, x) -> (name, x)) args in
  (env, args)
