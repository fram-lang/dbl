(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types and related constructs. *)

open Common

let tr_ceffect env (eff : S.effct) =
  match eff with
  | Pure   -> T.Pure
  | Impure -> T.Impure (Env.fresh_gvar env)

let rec tr_type env (tp : S.typ) =
  match S.Type.view tp with
  | TEffect -> T.Type.t_effect (Env.fresh_gvar env)

  | TUVar u ->
    begin match S.Kind.view (S.Type.kind tp) with
    | KEffect ->
      let _ : Scope.t = S.UVar.raw_set u S.Type.t_effect in
      T.Type.t_effect (Env.fresh_gvar env)

    | _ ->
      (* TODO: we can handle them in the future. *)
      Error.fatal (Error.unsolved_unification_variable ~pos:(S.UVar.pos u))
    end

  | TVar x -> T.Type.t_var (Env.lookup_tvar env x)

  | TArrow(sch, tp, eff) ->
    T.Type.t_arrow (tr_scheme env sch) (tr_type env tp) (tr_ceffect env eff)

  | THandler(x, cap_tp, in_tp, out_tp) ->
    let out_tp  = tr_type env out_tp in
    let out_eff = Env.fresh_gvar env in
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, x) = Env.add_tvar env x in
    let cap_tp = tr_type env cap_tp in
    let in_tp  = tr_type env in_tp in
    let in_eff = T.Effct.join (T.Effct.var x) (Env.fresh_gvar env) in
    let tp = T.Type.t_handler x cap_tp in_tp in_eff out_tp out_eff in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:[x]
      (Env.constraints env) (T.Scheme.of_type tp);
    tp

  | TLabel delim_tp ->
    T.Type.t_label
      (Env.fresh_gvar env)
      (tr_type env delim_tp)
      (Env.fresh_gvar env)

  | TApp(tp1, tp2) ->
    T.Type.t_app
      (tr_type env tp1)
      (tr_type env tp2)

and tr_scheme env (sch : S.scheme) =
  match sch.sch_targs with
  | [] ->
    { T.sch_targs = [];
      T.sch_named = List.map (tr_named_scheme env) sch.sch_named;
      T.sch_body  = tr_type env sch.sch_body
    }
  | targs ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, targs) = Env.add_named_tvars env sch.sch_targs in
    let sch =
      { T.sch_targs = targs;
        T.sch_named = List.map (tr_named_scheme env) sch.sch_named;
        T.sch_body  = tr_type env sch.sch_body
      } in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:(List.map snd targs)
      (Env.constraints env) sch;
    sch

and tr_named_scheme env (name, sch) =
  (name, tr_scheme env sch)

let rec tr_type_expr env (tp : S.type_expr) =
  match tp.data with
  | TE_Type tp -> tr_type env tp

  | TE_Effect tps ->
    T.Type.t_effect
      (List.fold_left
        (fun eff tp ->
          T.Effct.join eff (tr_effect_expr env tp))
        T.Effct.pure
        tps)

  | TE_PureArrow(sch, tp) ->
    T.Type.t_arrow (tr_scheme_expr env sch) (tr_type_expr env tp) Pure

  | TE_Arrow(sch, tp, eff) ->
    T.Type.t_arrow
      (tr_scheme_expr env sch)
      (tr_type_expr env tp)
      (Impure (tr_effect_expr env eff))

  | TE_Handler h ->
    let out_type = tr_type_expr env h.out_type in
    let out_eff  = tr_effect_expr env h.out_eff in
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, x) = Env.add_tvar env h.eff_var in
    let cap_type = tr_type_expr env h.cap_type in
    let in_type  = tr_type_expr env h.in_type in
    let in_eff   = tr_effect_expr env h.in_eff in
    let tp = T.Type.t_handler x cap_type in_type in_eff out_type out_eff in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:[x]
      (Env.constraints env) (T.Scheme.of_type tp);
    tp

  | TE_Label { eff; delim_tp; delim_eff } ->
    T.Type.t_label
      (tr_effect_expr env eff)
      (tr_type_expr env delim_tp)
      (tr_effect_expr env delim_eff)

  | TE_App(tp1, tp2) ->
    T.Type.t_app
      (tr_type_expr env tp1)
      (tr_type_expr env tp2)

  | TE_Option tp ->
    T.Type.t_app
      (T.Type.t_var T.BuiltinType.tv_option)
      (tr_type_expr env tp)

and tr_effect_expr env eff =
  T.Type.to_effect (tr_type_expr env eff)

and tr_scheme_expr env (se : S.scheme_expr) =
  match se.se_targs with
  | [] ->
    { T.sch_targs = [];
      T.sch_named = List.map (tr_named_scheme_expr env) se.se_named;
      T.sch_body  = tr_type_expr env se.se_body
    }

  | _ ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, targs) = Env.add_named_tvars env se.se_targs in
    let sch =
      { T.sch_targs = targs;
        T.sch_named = List.map (tr_named_scheme_expr env) se.se_named;
        T.sch_body  = tr_type_expr env se.se_body
      } in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:(List.map snd targs)
      (Env.constraints env) sch;
    sch

and tr_named_scheme_expr env (name, sch) =
  (name, tr_scheme_expr env sch)
