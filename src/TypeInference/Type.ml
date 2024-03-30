(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kind-checking and translation of type expressions *)

open Common

let rec infer_kind env (tp : S.type_expr) =
  match tp.data with
  | TWildcard ->
    let k = T.Kind.fresh_uvar ~non_effect:true () in
    (Env.fresh_uvar env k, k)

  | TVar x ->
    begin match Env.lookup_tvar env x with
    | Some tp -> (tp, T.Type.kind tp)
    | None ->
      Error.report (Error.unbound_type_var ~pos:tp.pos x);
      let k = T.Kind.fresh_uvar ~non_effect:true () in
      (Env.fresh_uvar env k, k)
    end

  | TPureArrow(sch, tp) ->
    let sch = tr_scheme env sch in
    let tp  = tr_ttype  env tp  in
    (T.Type.t_pure_arrow sch tp, T.Kind.k_type)

  | TArrow(sch, tp, eff) ->
    let sch = tr_scheme env sch in
    let tp  = tr_ttype  env tp  in
    let eff = tr_effrow env eff in
    (T.Type.t_arrow sch tp eff, T.Kind.k_type)

  | TEffect(tps, ee) ->
    (check_effrow env tps ee, T.Kind.k_effrow)

  | TApp(tp1, tp2) ->
    let pos1 = tp1.pos in
    let (tp1, k1) = infer_kind env tp1 in
    begin match Unification.kind_to_arrow k1 with
    | Some(k2, kv) ->
      let tp2 = check_kind env tp2 k2 in
      (T.Type.t_app tp1 tp2, kv)
    | None ->
      Error.fatal (Error.type_not_function ~pos:pos1 k1)
    end

and check_kind env (tp : S.type_expr) k =
  match tp.data with
  | TVar _ | TPureArrow _ | TArrow _ | TApp _ ->
    check_kind_default env tp k

  | TEffect(tps, ee) ->
    begin match T.Kind.view k, ee with
    | KEffect, None -> T.Type.t_effect (check_effect env tps)
    | _ -> check_kind_default env tp k
    end

  | TWildcard ->
    if T.Kind.set_non_effect k then
      Env.fresh_uvar env k
    else
      Error.fatal (Error.wildcard_in_effect ~pos:tp.pos)

and check_kind_default env tp k =
  let (tp', k') = infer_kind env tp in
  match T.Kind.view k', T.Kind.view k with
  | KEffect, KEffrow ->
    (* effects can be implicitly coerced to effect rows. *)
    T.Type.t_closed_effrow (T.Type.effect_view tp')
  | _ ->
    if Unification.unify_kind k k' then
      tp'
    else
      Error.fatal (Error.kind_mismatch ~pos:tp.pos k' k);

and check_effrow env tps ee =
  let tvs = check_effect env tps in
  match ee with
  | None    -> T.Type.t_closed_effrow tvs
  | Some ee ->
    let (tvs', ee) = T.Type.effrow_view (check_kind env ee T.Kind.k_effrow) in
    T.Type.t_effrow (T.TVar.Set.union tvs tvs') ee

and check_effect env tps =
  List.fold_left (check_effect_it env) T.TVar.Set.empty tps

and check_effect_it env tvs tp =
  T.TVar.Set.union tvs
    (T.Type.effect_view (check_kind env tp T.Kind.k_effect))

and tr_scheme env (sch : S.scheme_expr) =
  let (env, tvs) = tr_named_type_args env sch.sch_targs in
  let named = List.map (tr_named_scheme env) sch.sch_named in
  { T.sch_targs = tvs;
    T.sch_named = named;
    T.sch_body  = tr_ttype env sch.sch_body
  }

and tr_named_scheme env (nsch : S.named_scheme) =
  let (name, sch) = nsch.data in
  let sch = tr_scheme env sch in
  match name with
  | NLabel      ->
    let { T.sch_targs; sch_named; sch_body } = sch in
    if not (List.is_empty sch_targs && List.is_empty sch_named) then
      Error.fatal (Error.polymorphic_label ~pos:nsch.pos);
    begin match Unification.as_label env sch_body with
    | L_Label _  -> (T.NLabel, sch)
    | L_NoEffect -> Error.fatal (Error.cannot_guess_label_effect ~pos:nsch.pos)
    | L_No       -> Error.fatal (Error.label_type_mismatch ~pos:nsch.pos)
    end
  | NVar      x -> (T.NVar x, sch)
  | NImplicit n -> (T.NImplicit n, sch)

and tr_ttype env tp =
  check_kind env tp T.Kind.k_type

and tr_effrow env eff =
  check_kind env eff T.Kind.k_effrow

and tr_type_arg env (arg : S.type_arg) =
  match arg.data with
  | TA_Effect -> Env.add_the_effect ~pos:arg.pos env
  | TA_Var x -> Env.add_tvar ~pos:arg.pos env x (T.Kind.fresh_uvar ())

and check_type_arg env (arg : S.type_arg) kind =
  match arg.data with
  | TA_Effect ->
    let (env, x) = Env.add_the_effect ~pos:arg.pos env in
    if not (Unification.unify_kind kind T.Kind.k_effect) then
      Error.fatal (Error.effect_arg_kind_mismatch ~pos:arg.pos kind);
    (env, x)
  | TA_Var x -> Env.add_tvar ~pos:arg.pos env x kind

and tr_named_type_arg env (arg : S.named_type_arg) =
  let name = Name.tr_tname (fst arg.data) in
  let (env, x) = tr_type_arg env (snd arg.data) in
  let k = T.TVar.kind x in
  begin match name with
  | TNAnon ->
    if not (T.Kind.set_non_effect k) then
      Error.fatal (Error.anon_effect_arg ~pos:arg.pos)
  | TNEffect ->
    if not (Unification.unify_kind k T.Kind.k_effect) then
      Error.fatal (Error.effect_arg_kind_mismatch ~pos:arg.pos k)
  | TNVar _ -> ()
  end;
  (env, (name, x))

and tr_named_type_args env args =
  Uniqueness.check_named_type_arg_uniqueness args;
  List.fold_left_map tr_named_type_arg env args

let check_type_alias_binder env (arg : S.type_arg) tp =
  let kind = T.Type.kind tp in
  match arg.data with
  | TA_Effect ->
    if not (Unification.unify_kind kind T.Kind.k_effect) then
      Error.fatal (Error.effect_arg_kind_mismatch ~pos:arg.pos kind);
    Env.add_the_effect_alias env tp
  | TA_Var x ->
    Env.add_type_alias env x tp

let check_type_alias_binder_opt env arg_opt tp =
  match arg_opt with
  | None -> env
  | Some arg -> check_type_alias_binder env arg tp

let check_type_inst env targs (inst : S.type_inst) =
  let name = Name.tr_tname (fst inst.data) in
  match List.assoc_opt name targs with
  | None ->
    Error.warn (Error.redundant_named_type ~pos:inst.pos name);
    let _ : T.typ = check_kind env (snd inst.data) (T.Kind.fresh_uvar ()) in
    None
  | Some x ->
    let tp = check_kind env (snd inst.data) (T.TVar.kind x) in
    Some (name, tp)

let check_type_insts env tinst targs =
  List.filter_map (check_type_inst env targs) tinst
