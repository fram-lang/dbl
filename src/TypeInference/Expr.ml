(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for expressions and related syntactic categories *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Return information about effect. *)
type ret_effect =
  | Pure
    (** Expression is pure, i.e, it does not perform any effects, and always
      terminates *)

  | Impure
    (** Expression is inpure *)

let ret_effect_join eff1 eff2 =
  match eff1, eff2 with
  | Pure,   eff2   -> eff2
  | eff1,   Pure   -> eff1
  | Impure, Impure -> Impure

(* ------------------------------------------------------------------------- *)
(** Infer scheme of type variable *)
let infer_var_scheme ~pos env x =
  match Env.lookup_var env x with
  | Some (x, sch) ->
    ({ T.pos = pos; T.data = T.EVar x }, sch)
  | None ->
    Error.fatal (Error.unbound_var ~pos x)

(* ------------------------------------------------------------------------- *)
(** Infer scheme of a named implicit *)
let infer_implicit_scheme ~pos env name =
  match Env.lookup_implicit env name with
  | Some (x, sch, on_use) ->
    on_use pos;
    ({ T.pos = pos; T.data = T.EVar x }, sch)
  | None ->
    Error.fatal (Error.unbound_implicit ~pos name)

(* ------------------------------------------------------------------------- *)
(** Infer type of an expression. The effect of an expression is always in
  the check mode. However, pure expressions may returns an information that
  they are pure (see [ret_effect] type). *)
let rec infer_expr_type env (e : S.expr) eff =
  let pos = e.pos in
  let make data = { e with data = data } in
  match e.data with
  | EUnit ->
    (make T.EUnit, T.Type.t_unit, Pure)

  | EVar x ->
    let (e, sch) = infer_var_scheme ~pos env x in
    let (e, tp) = ExprUtils.instantiate env e sch in
    (e, tp, Pure)

  | EName n ->
    let (e, sch) = infer_implicit_scheme ~pos env n in
    let (e, tp)  = ExprUtils.instantiate env e sch in
    (e, tp, Pure)

  | EFn(x, body) ->
    let tp1 = Env.fresh_uvar env T.Kind.k_type in
    let (env, x) = Env.add_mono_var env x tp1 in
    let body_eff = Env.fresh_uvar env T.Kind.k_effect in
    let (body, tp2, r_eff) = infer_expr_type env body body_eff in
    begin match r_eff with
    | Pure ->
      (make (T.EPureFn(x, tp1, body)), T.Type.t_pure_arrow tp1 tp2, Pure)
    | Impure ->
      (make (T.EFn(x, tp1, body)), T.Type.t_arrow tp1 tp2 body_eff, Pure)
    end

  | EApp(e1, e2) ->
    let (e1, ftp, r_eff1) = infer_expr_type env e1 eff in
    begin match Unification.to_arrow env ftp with
    | Arr_Pure(atp, vtp) ->
      let (e2, r_eff2) = check_expr_type env e2 atp eff in
      (make (T.EApp(e1, e2)), vtp, ret_effect_join r_eff1 r_eff2)
    | Arr_Impure(atp, vtp, f_eff) ->
      let (e2, r_eff2) = check_expr_type env e2 atp eff in
      if not (Unification.subeffect env f_eff eff) then
        Error.report (Error.func_effect_mismatch ~pos:e1.pos ~env f_eff eff);
      (make (T.EApp(e1, e2)), vtp, Impure)
    | Arr_No ->
      Error.fatal (Error.expr_not_function ~pos:e1.pos ~env ftp)
    end

  | EDefs(defs, e) ->
    let scope = Env.scope env in
    let (env, e_gen, r_eff1) = check_defs env ImplicitEnv.empty defs eff in
    let (e, tp, r_eff2) = infer_expr_type env e eff in
    (* TODO: tp can be raised to some supertype in the scope *)
    begin match T.Type.try_shrink_scope ~scope tp with
    | Ok   () -> (e_gen e, tp, ret_effect_join r_eff1 r_eff2)
    | Error x ->
      Error.fatal (Error.type_escapes_its_scope ~pos ~env x)
    end

  | EHandle(x, e1, h) ->
    (* Since type and effect of e1 is used both on covariant and contravariant
     position (return type and resumption respectively), we should guess
     them even in type-check mode. *)
    let res_tp  = Env.fresh_uvar env T.Kind.k_type   in
    let res_eff = Env.fresh_uvar env T.Kind.k_effect in
    let (env1, h_eff) = Env.add_anon_tvar env T.Kind.k_cleffect in
    (* TODO: effect capability may have a scheme instead of type *)
    let (h, x_tp) = infer_h_expr_type env h h_eff res_tp res_eff in
    let (env1, x) = Env.add_mono_var env1 x x_tp in
    let (e1, _) = check_expr_type env1 e1 res_tp (T.Effect.cons h_eff res_eff) in
    if not (Unification.subeffect env res_eff eff) then
      Error.report (Error.expr_effect_mismatch ~pos ~env res_eff eff);
    (make (T.EHandle(h_eff, x, e1, h, res_tp, res_eff)), res_tp, Impure)

  | ERepl func ->
    let tp = T.Type.t_unit in
    let e =
      make (T.ERepl(
        (fun () -> fst (check_expr_type env (func ()) tp eff)),
        eff))
    in (e, tp, Impure)

  | EReplExpr(e1, e2) ->
    let (e1, tp1, r_eff1) = check_repl_expr env e1 eff in
    let (e2, tp2, r_eff2) = infer_expr_type env e2 eff in
    (make (T.EReplExpr(e1, tp1, e2)), tp2, ret_effect_join r_eff1 r_eff2)

(* ------------------------------------------------------------------------- *)
(** Check type and effect of an expression. Returns also information about
  the purity of an expression. *)
and check_expr_type env (e : S.expr) tp eff =
  let make data = { e with data = data } in
  match e.data with
  | EUnit | EVar _ | EName _ | EApp _ | EHandle _ | ERepl _ ->
    let pos = e.pos in
    let (e, tp', r_eff) = infer_expr_type env e eff in
    if not (Unification.subtype env tp' tp) then
      Error.report (Error.expr_type_mismatch ~pos ~env tp' tp);
    (e, r_eff)

  | EFn(x, body) ->
    begin match Unification.from_arrow env tp with
    | Arr_Pure(tp1, tp2) ->
      let (env, x) = Env.add_mono_var env x tp1 in
      let (body, r_eff) = check_expr_type env body tp2 T.Effect.pure in
      if r_eff <> Pure then
        Error.report (Error.func_not_pure ~pos:e.pos);
      (make (T.EPureFn(x, tp1, body)), Pure)
    | Arr_Impure(tp1, tp2, eff) ->
      let (env, x) = Env.add_mono_var env x tp1 in
      let (body, _) = check_expr_type env body tp2 eff in
      (make (T.EFn(x, tp1, body)), Pure)
    | Arr_No ->
      Error.report (Error.expr_not_function_ctx ~pos:e.pos ~env tp);
      let (e, _, r_eff) = infer_expr_type env e eff in
      (e, r_eff)
    end

  | EDefs(defs, e) ->
    let (env, e_gen, r_eff1) = check_defs env ImplicitEnv.empty defs eff in
    let (e, r_eff2) = check_expr_type env e tp eff in
    (e_gen e, ret_effect_join r_eff1 r_eff2)

  | EReplExpr(e1, e2) ->
    let (e1, tp1, r_eff1) = check_repl_expr env e1 eff in
    let (e2, r_eff2) = check_expr_type env e2 tp eff in
    (make (T.EReplExpr(e1, tp1, e2)), ret_effect_join r_eff1 r_eff2)

(* ------------------------------------------------------------------------- *)
(** Check effect of a block of definitions. Returns extended environment, a
  function that extends expression with given list of definitions, and the
  effect of a definition block. *)
and check_defs env ienv (defs : S.def list) eff =
  match defs with
  | [] -> (env, (fun e -> e), Pure)
  | def :: defs ->
    let (env, ienv, e_gen1, r_eff1) = check_def env ienv def eff in
    let (env, e_gen2, r_eff2) = check_defs env ienv defs eff in
    (env, (fun e -> e_gen1 (e_gen2 e)), ret_effect_join r_eff1 r_eff2)

and check_def env ienv (def : S.def) eff =
  let make (e : T.expr) data =
    { T.pos  = Position.join def.pos e.pos;
      T.data = data
    } in
  match def.data with
  | DLet(x, e1) ->
    let (sch, e1, r_eff) = check_let env ienv e1 eff in
    let (env, x) = Env.add_poly_var env x sch in
    (env, ienv, (fun e -> make e (T.ELet(x, sch, e1, e))), r_eff)

  | DLetName(n, e1) ->
    let (sch, e1, r_eff) = check_let env ienv e1 eff in
    let ienv = ImplicitEnv.shadow ienv n in
    let (env, x) = Env.add_poly_implicit env n sch ignore in
    (env, ienv, (fun e -> make e (T.ELet(x, sch, e1, e))), r_eff)

  | DImplicit n ->
    let ienv = ImplicitEnv.declare_implicit ienv n in
    (env, ienv, (fun e -> e), Pure)

  | DData(name, ctors) ->
    Uniqueness.check_ctor_uniqueness ctors;
    let ctors = DataType.check_ctor_decls env ctors in
    let (env, x) = Env.add_tvar env name T.Kind.k_type in
    let px = Var.fresh ~name () in
    let proof = { T.pos = def.pos; T.data = T.EVar px } in
    let env = Env.add_data env x proof ctors in
    let env = DataType.open_data env (T.Type.t_var x) proof ctors in
    (env, ienv, (fun e -> make e (T.EData(x, px, ctors, e))), Pure)

(* ------------------------------------------------------------------------- *)
(** Check let-definition *)
and check_let env ienv body eff =
  let (body_env, ims) = ImplicitEnv.begin_generalize env ienv in
  let (body, tp, r_eff) = infer_expr_type body_env body eff in
  (* Purity restriction: check if r_eff is pure. If so, then generalize type
    of an expression *)
  match r_eff with
  | Pure ->
    let ims = ImplicitEnv.end_generalize_pure ims in
    let (body, sch) = ExprUtils.generalize env ims body tp in
    (sch, body, Pure)
  | Impure ->
    ImplicitEnv.end_generalize_impure ims;
    let sch = { T.sch_tvars = []; T.sch_implicit = []; T.sch_body = tp } in
    (sch, body, Impure)

(* ------------------------------------------------------------------------- *)
(** Infer type of an handler expression.
  In [infer_h_expr_type env h h_eff res_tp res_eff] the parameters have the
  following meaning:
  - [env]     -- an environment
  - [h]       -- handler expression
  - [h_eff]   -- a handled effect
  - [res_tp]  -- returned type
  - [res_eff] -- returned effect *)
and infer_h_expr_type env h h_eff res_tp res_eff =
  let make data = { h with data = data } in
  match h.data with
  | HEffect(x, r, body) ->
    let in_tp  = Env.fresh_uvar env T.Kind.k_type in
    let out_tp = Env.fresh_uvar env T.Kind.k_type in
    let r_tp   = T.Type.t_arrow out_tp res_tp res_eff in
    let (env, x) = Env.add_mono_var env x in_tp in
    let (env, r) = Env.add_mono_var env r r_tp in
    let (body, _) = check_expr_type env body res_tp res_eff in
    (make (T.HEffect(in_tp, out_tp, x, r, body)),
      T.Type.t_arrow in_tp out_tp (T.Effect.singleton h_eff))

(* ------------------------------------------------------------------------- *)
(** Check expression put into REPL *)
and check_repl_expr env e eff =
  let (e, tp, r_eff) = infer_expr_type env e eff in
  (e, "?", r_eff)
