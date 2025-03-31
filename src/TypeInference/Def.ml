(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for definitions *)

open Common
open BiDirectional
open TypeCheckFix


let switch_to_check_mode (type dir) env (req : (T.typ, dir) request) :
    T.typ * (T.typ, dir) response =
  match req with
  | Infer ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    (tp, Infered tp)
  | Check tp -> (tp, Checked)

(* ------------------------------------------------------------------------- *)
let check_def : type st dir. tcfix:tcfix ->
  (st, sec) opn Env.t -> S.def -> (T.typ, dir) request ->
    st def_cont -> dir expr_result =
  fun ~tcfix env def req cont ->
  let open (val tcfix : TCFix) in
  let make (rest : _ expr_result) data =
    { T.pos  = Position.join def.pos rest.er_expr.pos;
      T.pp   = Env.pp_tree env;
      T.data = data
    } in
  let make_local data =
    { T.pos  = def.pos;
      T.pp   = Env.pp_tree env;
      T.data = data
    } in
  let pos = def.pos in
  let pp = Env.pp_tree env in
  match def.data with
  | DLetId(public, id, body) ->
    let (body_env, params) = Env.begin_generalize env in
    begin match PolyExpr.infer_def_scheme ~tcfix body_env body with
    | PPure(body, sch, cs) ->
      let cs = ConstrSolve.solve_partial cs in
      let (targs, named, cs) =
        ParamGen.end_generalize_pure ~pos params (Name.scheme_uvars sch) cs in
      let (body, sch) = ExprUtils.generalize ~pos ~pp targs named body sch in
      let name = NameUtils.tr_ident ~pos ~pp id sch in
      let (env, x) = Env.add_val ~public env name sch in
      let rest = cont.run env req in
      { er_expr   = make rest (T.ELetPoly(x, body, rest.er_expr));
        er_type   = rest.er_type;
        er_effect = rest.er_effect;
        er_constr = cs @ rest.er_constr
      }
    | PImpure body ->
      let body_tp = expr_result_type body in
      ParamGen.end_generalize_impure params (T.Type.uvars body_tp);
      let sch = T.Scheme.of_type body_tp in
      let name = NameUtils.tr_ident ~pos ~pp id sch in
      let (env, x) = Env.add_val ~public env name sch in
      let rest = cont.run env req in
      { er_expr   = make rest (T.ELetMono(x, body.er_expr, rest.er_expr));
        er_type   = rest.er_type;
        er_effect = T.Effect.join body.er_effect rest.er_effect;
        er_constr = body.er_constr @ rest.er_constr
      }
    end

  | DLetPat(pat, body) ->
    (* Pattern may introduce type variables. We switch to check-mode in
      order to make sure that they don't escape their scope. *)
    let (tp, resp) = switch_to_check_mode env req in
    let (body_env, params) = Env.begin_generalize env in
    let body = infer_expr_type body_env body in
    let body_tp = expr_result_type body in
    let (penv, pat, pat_eff) = Pattern.check_type env pat body_tp in
    let (env, _, _, ren) = PartialEnv.extend env [] penv in
    let pat = T.Ren.rename_pattern ren pat in
    let rest = cont.run env (Check tp) in
    let eff = T.Effect.joins [ body.er_effect; pat_eff; rest.er_effect ] in
    let expr = make rest
      (T.EMatch(body.er_expr, [(pat, rest.er_expr)], tp, eff)) in
    { er_expr   = expr;
      er_type   = resp;
      er_effect = eff;
      er_constr = body.er_constr @ rest.er_constr
    }

  | DLabel(eff, pat) ->
    let (tp, resp) = switch_to_check_mode env req in
    let delim_tp = Env.fresh_uvar env T.Kind.k_type in
    let (env, _) = Env.enter_scope env in
    let (env, name, x) = Type.check_type_arg env eff T.Kind.k_effect in
    let (env, pat, pat_eff) =
      Pattern.check_type_ext env pat (T.Type.t_label delim_tp) in
    let rest = cont.run env (Check tp) in
    let rest_eff = T.Effect.join pat_eff rest.er_effect in
    let lx = Var.fresh ~name:"lbl" () in
    let dd = T.DD_Label
      { tvar     = x;
        var      = lx;
        delim_tp = delim_tp;
        annot    = make_local (T.TE_Type delim_tp)
      } in
    let expr =
      make rest (T.EData([dd],
        make rest (T.EMatch(
          make_local (T.EInst(make_local (T.EVar lx), [], [])),
          [(pat, rest.er_expr)], tp, rest_eff)))) in
    { er_expr   = expr;
      er_type   = resp;
      er_effect = Impure;
      er_constr = rest.er_constr
    }

  | DHandlePat(pat, heff, hexp) ->
    let env0 = env in
    let (hexp_env, params) = Env.begin_generalize env in
    let hexp = infer_expr_type hexp_env hexp in
    let hexp_tp = expr_result_type hexp in
    ParamGen.end_generalize_impure params (T.Type.uvars hexp_tp);
    begin match Unification.to_handler env hexp_tp with
    | H_Handler(a, cap_tp, tp_in, tp_out) ->
      let (env, scope) = Env.enter_scope env in
      let (env, name, eff_tvar) =
        Type.check_type_arg env heff T.Kind.k_effect in
      let sub = T.Subst.rename_tvar (T.Subst.empty ~scope) a eff_tvar in
      let cap_tp = T.Type.subst sub cap_tp in
      let tp_in  = T.Type.subst sub tp_in in
      let (env, pat, _) = Pattern.check_type_ext env pat cap_tp in
      let rest = cont.run env (Check tp_in) in
      let hx = Var.fresh ~name:"cap" () in
      let expr =
        make rest (T.EHandle(eff_tvar, hx, hexp.er_expr,
          make rest (T.EMatch(
            make_local (T.EInst(make_local (T.EVar hx), [], [])),
            [(pat, rest.er_expr)], tp_in, Impure)))) in
      let resp : (_, dir) response =
        match req with
        | Infer    -> Infered tp_out
        | Check tp ->
          let pp = Env.pp_tree env0 in
          Error.check_unify_result ~pos
            (Unification.subtype env0 tp_out tp)
            ~on_error:(Error.expr_type_mismatch ~pp tp_out tp);
          Checked
      in
      { er_expr   = expr;
        er_type   = resp;
        er_effect = Impure;
        er_constr = hexp.er_constr @ rest.er_constr
      }
    | H_No ->
      Error.fatal (Error.expr_not_handler ~pos:hexp.er_expr.pos ~pp hexp_tp)
    end

  | DTypeParam(name, x, kind) ->
    let kind = Type.tr_kind kind in
    let name = tr_tname name in
    let env = Env.declare_type ~pos env name x kind in
    cont.run env req

  | DValParam(name, x, sch) ->
    let (sch_env, params) = Env.begin_generalize env in
    let sch =
      match sch with
      | Some sch -> Type.tr_scheme sch_env sch
      | None     ->
        let tp = make_local (T.TE_Type (Env.fresh_uvar env T.Kind.k_type)) in
        T.SchemeExpr.of_type_expr tp
    in
    let env = ParamGen.end_generalize_declare ~pos params env name x sch in
    cont.run env req

  | DData { public_tp; public_ctors; tvar=name; args; ctors } ->
    let (tp, resp) = switch_to_check_mode env req in
    let nonrec_scope = Env.scope env in
    let (penv, args) = Type.tr_named_type_args args in
    let kind = DataType.kind args in
    let (data_env, _, args, _) = PartialEnv.extend env args penv in
    let ctors = DataType.check_ctor_decls ~data_targs:args data_env ctors in
    let (env, _) = Env.enter_scope env in
    let (env, x) = Env.add_tvar ~pos ~public:public_tp env name kind in
    let (env, dd) =
      DataType.finalize_check ~nonrec_scope ~public:public_ctors
        env x ~name args ctors in
    let rest = cont.run env (Check tp) in
    { er_expr   = make rest (T.EData([dd], rest.er_expr));
      er_type   = resp;
      er_effect = rest.er_effect;
      er_constr = rest.er_constr
    }

  | DBlock defs ->
    let env = Env.enter_section env in
    check_defs env defs req
      { run = fun env req ->
        cont.run (Env.leave_section env) req }

  | DRec defs ->
    let env = Env.enter_section env in
    let (tp, resp) = switch_to_check_mode env req in
    let result = RecDefs.check_rec_defs ~tcfix ~pos env defs in
    let env = Env.leave_section result.rec_env in
    let rest = cont.run env (Check tp) in
    { er_expr   =
        make rest (T.EData(result.rec_dds,
          make rest (T.ELetRec
            { targs = result.rec_targs;
              named = result.rec_named;
              defs  = result.rec_fds;
              body  = rest.er_expr
            })));
      er_type   = resp;
      er_effect = T.Effect.join result.rec_eff rest.er_effect;
      er_constr = result.rec_constr @ rest.er_constr
    }

  | DModule(public, name, defs) ->
    let env = Env.enter_module env in
    let env = Env.enter_section env in
    check_defs env defs req
      { run = fun env req ->
        let env = Env.leave_section env in
        let env = Env.leave_module env ~public name in
        cont.run env req }

  | DOpen(public, path) ->
    let m = ModulePath.lookup_module env path in
    let env = Env.open_module ~public env m in
    cont.run env req

  | DReplExpr e ->
    let (body_env, params) = Env.begin_generalize env in
    let expr = infer_expr_type body_env e in
    let tp = expr_result_type expr in
    ParamGen.end_generalize_impure params (T.Type.uvars tp);
    let rest = cont.run env req in
    { er_expr   = make rest (T.EReplExpr(expr.er_expr, rest.er_expr));
      er_type   = rest.er_type;
      er_effect = T.Effect.join expr.er_effect rest.er_effect;
      er_constr = expr.er_constr @ rest.er_constr
    }

(* ------------------------------------------------------------------------- *)
let check_defs : type st dir. tcfix:tcfix ->
  (st, sec) opn Env.t -> S.def list -> (T.typ, dir) request ->
    st def_cont -> dir expr_result =
  fun ~tcfix env defs req cont ->
  let open (val tcfix : TCFix) in
  match defs with
  | [] -> cont.run env req
  | def :: defs ->
    check_def env def req
      { run = fun env req -> check_defs env defs req cont }
