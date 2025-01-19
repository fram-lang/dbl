(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scheme-inference for polymorphic expressions *)

open Common
open TypeCheckFix

type check_def_result =
  | Mono of check expr_result
  | Poly of T.poly_expr * Constr.t list

type infer_def_result =
  | PPure of T.poly_expr * Name.scheme * Constr.t list
  | PImpure of infer expr_result

type inst_context =
  | Empty
  | InstCtx of (infer expr_result -> infer expr_result)

let plug_inst_context ctx expr =
  match ctx with
  | Empty     -> expr
  | InstCtx f -> f expr

(* ------------------------------------------------------------------------- *)
(** Context of a method call, returned by [infer_poly_scheme] *)
let method_call_ctx pos env self =
  InstCtx (fun expr ->
    let result_expr =
      { T.pos = pos; T.data = T.EAppMono(expr.er_expr, self.er_expr) } in
    let self_tp = expr_result_type self in
    match T.Type.view (expr_result_type expr) with
    | TArrow(
        { sch_targs = []; sch_named = []; sch_body = self_tp' },
        res_tp, res_eff) ->
      let pp = Env.pp_tree env in
      Error.check_unify_result ~pos:self.er_expr.pos
        (Unification.subtype env self_tp self_tp')
        ~on_error:(Error.expr_type_mismatch ~pp self_tp self_tp');
      { er_expr   = result_expr;
        er_type   = Infered res_tp;
        er_effect = T.Effect.joins [ self.er_effect; expr.er_effect; res_eff ];
        er_constr = self.er_constr @ expr.er_constr
      }

    | _ ->
      (* Method must be an arrow with monomorphic argument *)
      InterpLib.InternalError.report ~reason:"invalid method type" ())

(* ------------------------------------------------------------------------- *)
let lookup_method ~pos env self_tp name =
  match NameUtils.method_owner_of_self self_tp with
  | Some own ->
    begin match ModulePath.try_lookup_method ~pos env own name with
    | Some(x, sch) ->
      let poly_expr = { T.pos = pos; T.data = T.EVar x } in
      (poly_expr, sch)
    | None ->
      let pp = Env.pp_tree env in
      Error.fatal (Error.unbound_method ~pos ~pp own name)
    end

  | None ->
    (* TODO: create a method constraint *)
    failwith "Not implemented: method call on unknown type"

(* ------------------------------------------------------------------------- *)
let infer_use_scheme ~tcfix ?app_type env (e : S.poly_expr_use) =
  let open (val tcfix : TCFix) in
  let pos = e.pos in
  match e.data with
  | EVar path ->
    let (poly_expr, sch) = ModulePath.lookup_var env path in
    (Empty, poly_expr, sch)

  | EImplicit path ->
    let (poly_expr, sch) = ModulePath.lookup_implicit env path in
    (Empty, poly_expr, sch)

  | EMethod(self, name) ->
    let self = infer_expr_type env self in
    let self_tp = expr_result_type self in
    let (poly_expr, sch) = lookup_method ~pos env self_tp name in
    (method_call_ctx pos env self, poly_expr, sch)

(* ------------------------------------------------------------------------- *)
let check_def_scheme ~tcfix env (e : S.poly_expr_def) (sch : T.scheme) =
  let open (val tcfix : TCFix) in
  let pos = e.pos in
  let pp = Env.pp_tree env in
  let make data = { T.data; T.pos } in
  match sch.sch_targs, sch.sch_named, e.data with
  | [], [], PE_Expr expr -> Mono (check_expr_type env expr sch.sch_body)

  | _, _, PE_Expr expr ->
    let (env, _, targs, named, body_tp) =
      ParamResolve.open_scheme ~pos env sch in
    let expr = check_expr_type env expr body_tp in
    begin match expr.er_effect with
    | Pure   -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos)
    end;
    let poly_expr = make (T.EPolyFun(targs, named, expr.er_expr)) in
    Poly(poly_expr, expr.er_constr)

  | _, _, PE_Poly poly_expr ->
    let (env, rctx, targs, named, body_tp) =
      ParamResolve.open_scheme ~pos env sch in
    let (ictx, poly_expr, sch') =
      infer_use_scheme ~tcfix ~app_type:body_tp env poly_expr in
    let (expr, tp, cs) =
      ParamResolve.instantiate ~pos env rctx poly_expr sch' in
    let expr = plug_inst_context ictx
      { er_expr   = expr;
        er_type   = Infered tp;
        er_effect = Pure;
        er_constr = cs
      } in
    let expr_tp = expr_result_type expr in
    Error.check_unify_result ~pos
      (Unification.subtype env expr_tp body_tp)
      ~on_error:(Error.expr_type_mismatch ~pp expr_tp body_tp);
    begin match targs, named with
    | [], [] -> Mono { expr with er_type = Checked }
    | _, _ ->
      begin match expr.er_effect with
      | Pure   -> ()
      | Impure -> Error.report (Error.func_not_pure ~pos)
      end;
      let poly_expr = make (T.EPolyFun(targs, named, expr.er_expr)) in
      Poly(poly_expr, expr.er_constr)
    end

  | _, _, PE_Fn(pats, body) ->
    let (env, targs, named, body_tp) =
      ParamResolve.open_scheme_explicit ~pos env sch in
    let (env, pats, pat_eff) =
      Pattern.check_named_patterns_ext ~pos env pats targs named in
    let body = check_expr_type env body body_tp in
    let eff = T.Effect.join pat_eff body.er_effect in
    begin match eff with
    | Pure   -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos)
    end;
    let body_expr = ExprUtils.match_args pats body.er_expr body_tp eff in
    let targs = List.map snd targs in
    let named = List.map (fun (_, x, sch) -> (x, sch)) named in
    let poly_expr = make (T.EPolyFun(targs, named, body_expr)) in
    Poly(poly_expr, body.er_constr)

(* ------------------------------------------------------------------------- *)
let infer_def_result_of_expr ~pos expr =
  let make data = { T.data; T.pos } in
  match expr.er_effect with
  | Pure ->
    let poly_expr = make (T.EPolyFun([], [], expr.er_expr)) in
    let sch = Name.scheme_of_type (expr_result_type expr) in
    PPure(poly_expr, sch, expr.er_constr)

  | Impure -> PImpure expr

let infer_def_scheme ~tcfix env (e : S.poly_expr_def) =
  let open (val tcfix : TCFix) in
  let pos = e.pos in
  match e.data with
  | PE_Expr expr ->
    let expr = infer_expr_type env expr in
    infer_def_result_of_expr ~pos expr

  | PE_Poly poly_expr ->
    let (ictx, poly_expr, sch) = infer_use_scheme ~tcfix env poly_expr in
    begin match ictx with
    | Empty ->
      let sch = NameUtils.tr_scheme ~pos ~pp:(Env.pp_tree env) sch in
      PPure(poly_expr, sch, [])
    | InstCtx f ->
      let (expr, tp, cs) =
        ParamResolve.instantiate ~pos env ParamResolve.no_reinst
          poly_expr sch in
      let expr = 
        { er_expr   = expr;
          er_type   = Infered tp;
          er_effect = Pure;
          er_constr = cs
        } in
      infer_def_result_of_expr ~pos (f expr)
    end

  | PE_Fn(pats, body) ->
    let (env, scope, targs, named, eff) =
      Pattern.infer_named_patterns_ext env pats in
    let body_tp = T.Type.fresh_uvar ~scope T.Kind.k_type in
    let body = check_expr_type env body body_tp in
    let eff = T.Effect.join eff body.er_effect in
    begin match eff with
    | Pure   -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos)
    end;
    let named = named |>
      List.map (fun (name, pat, sch) ->
        let x = Var.fresh () in
        (name, x, pat, sch)) in
    let sch_named = List.map (fun (name, _, _, sch) -> (name, sch)) named in
    let match_named = List.map (fun (_, x, pat, _) -> (x, pat)) named in
    let body_expr =
      ExprUtils.match_args match_named body.er_expr body_tp eff in
    let sch =
      { Name.sch_targs = targs;
        Name.sch_named = sch_named;
        Name.sch_body  = body_tp
      } in
    let targs = List.map snd targs in
    let named = List.map (fun (_, x, _, sch) -> (x, sch)) named in
    let poly_expr = { T.pos; T.data = T.EPolyFun(targs, named, body_expr) } in
    PPure(poly_expr, sch, body.er_constr)
