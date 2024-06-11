(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scheme-inference for polymorphic expressions and related constructs:
  actual parameters, and explicit instantiations. *)

open Common
open TypeCheckFix

type inst_context =
  T.expr -> T.typ -> ret_effect -> T.expr * T.typ * ret_effect

type simple_context = T.expr -> T.expr

(* ------------------------------------------------------------------------- *)
(** Context of a method call, returned by [infer_poly_scheme] *)
let method_call_ctx ~pos ~env ~self ~self_tp ~self_r_eff ~eff =
  fun e method_tp r_eff ->
  let result_expr = { T.pos = pos; T.data = T.EApp(e, self) } in
  match T.Type.view method_tp with
  | TArrow(
      { sch_targs = []; sch_named = []; sch_body = self_tp' },
      res_tp, res_eff) ->
    Error.check_unify_result ~pos:self.pos
      (Unification.subtype env self_tp self_tp')
      ~on_error:(Error.expr_type_mismatch ~env self_tp self_tp');
    Error.check_unify_result ~pos
      (Unification.subeffect env res_eff eff)
      ~on_error:(Error.method_effect_mismatch ~env res_eff eff);
    (* The method is impure, so the result is impure too. *)
    (result_expr, res_tp, Impure)
  | TPureArrow(
      { sch_targs = []; sch_named = []; sch_body = self_tp' },
      res_tp) ->
    Error.check_unify_result ~pos:self.pos
      (Unification.subtype env self_tp self_tp')
      ~on_error:(Error.expr_type_mismatch ~env self_tp self_tp');
    (result_expr, res_tp, ret_effect_join self_r_eff r_eff)
  | _ ->
    (* Method must be an arrow with monomorphic argument *)
    InterpLib.InternalError.report ~reason:"invalid method type" ()

(* ------------------------------------------------------------------------- *)
(** Default instantiation context. *)
let default_ctx e tp r_eff = (e, tp, r_eff)

(* ------------------------------------------------------------------------- *)
(** Infer scheme of regular variable *)
let infer_var_scheme ~pos env x =
  match Env.lookup_var env x with
  | Some (VI_Var(x, sch)) ->
    ({ T.pos = pos; T.data = T.EVar x }, sch)
  | Some (VI_Ctor(idx, info)) ->
    let ctor = List.nth info.adt_ctors idx in
    let targs = info.adt_args @ ctor.ctor_targs in
    let sch = {
        T.sch_targs = targs;
        T.sch_named = ctor.ctor_named;
        T.sch_body  = T.Type.t_pure_arrows ctor.ctor_arg_schemes info.adt_type
      } in
    (ExprUtils.ctor_func ~pos idx info, sch)
  | Some (VI_MethodFn name) ->
    Error.fatal (Error.method_fn_without_arg ~pos x name)
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
let infer_scheme ~tcfix env (e : S.poly_expr) eff =
  let open (val tcfix : TCFix) in
  let pos = e.pos in
  match e.data with
  | EVar  x ->
    let (e, sch) = infer_var_scheme ~pos env x in
    (default_ctx, e, sch, T.TVar.Map.empty)
  | EImplicit n ->
    let (e, sch) = infer_implicit_scheme ~pos env n in
    (default_ctx, e, sch, T.TVar.Map.empty)
  | EMethod(self, name) ->
    let (self, self_tp, self_r_eff) = infer_expr_type env self eff in
    begin match T.Type.whnf self_tp with
    | Whnf_Neutral(NH_Var a, _) ->
      begin match Env.lookup_method env a name with
      | Some(x, sch) ->
        (method_call_ctx ~pos ~env ~self ~self_tp ~self_r_eff ~eff,
          { T.pos = pos; T.data = T.EVar x },
          sch,
          TypeHints.method_inst_hints sch self_tp)
      | None ->
        Error.fatal (Error.unbound_method ~pos ~env a name)
      end
    | Whnf_Neutral(NH_UVar _, _) ->
      Error.fatal (Error.method_call_on_unknown_type ~pos:e.pos)

    | Whnf_PureArrow _ | Whnf_Arrow _ | Whnf_Handler _
    | Whnf_Label _ ->
      Error.fatal (Error.method_call_on_invalid_type ~pos:e.pos ~env self_tp)

    | Whnf_Effect _ | Whnf_Effrow _ ->
      failwith "Internal kind error"
    end

(* ------------------------------------------------------------------------- *)
let check_actual_arg ~tcfix env (arg : S.expr) sch eff =
  let open (val tcfix : TCFix) in
  let (env, tvars, named, body_tp) =
    TypeUtils.open_scheme ~pos:arg.pos env sch in
  let (body, res_eff) =
    match tvars, named with
    | [], [] -> check_expr_type env arg body_tp eff
    | _ ->
      let (body, res_eff) = check_expr_type env arg body_tp T.Effect.pure in
      begin match res_eff with
      | Pure -> ()
      | Impure -> 
        print_endline "Test1";
        Error.report (Error.func_not_pure ~pos:arg.pos) 
      end;
      (body, Pure)
  in
  (ExprUtils.make_tfun tvars (ExprUtils.make_nfun named body), res_eff)

(* ------------------------------------------------------------------------- *)
let rec check_explicit_insts ~tcfix
    env named (insts : S.inst list) (cache : TypeHints.inst_cache) eff =
  let open (val tcfix : TCFix) in
  match insts with
  | [] -> (Fun.id, [], Pure)
  | { data = (n, e); pos } :: insts ->
    let make data = { T.data; T.pos } in
    let n = Name.tr_name env n in
    let (n, e, sch, r_eff1) =
      match T.Name.assoc n named with
      | Some sch ->
        begin match T.Name.assoc n cache with
        | None ->
          let (e, r_eff1) =
            check_actual_arg ~tcfix env e sch eff in
          (n, e, sch, r_eff1)
        | Some (e, tp, r_eff1) ->
          assert (T.Scheme.is_monomorphic sch);
          Error.check_unify_result ~pos:e.pos
            (Unification.subtype env tp sch.sch_body)
            ~on_error:(Error.named_param_type_mismatch ~env n tp sch.sch_body);
          (n, e, sch, r_eff1)
        end
      | None ->
        let res = 
          begin match n with
          | NVar x -> 
            begin match T.Name.assoc (T.NOptionalVar x) named with
            | Some sch ->
              assert (T.Scheme.is_monomorphic sch);
              let tp = PreludeTypes.extr_arg_tp ~env ~pos:e.pos sch.sch_body in
              let (e, r_eff1) = check_actual_arg ~tcfix env e 
                (T.Scheme.of_type tp) eff 
              in
              Some (T.NOptionalVar x, 
                PreludeTypes.mk_Some ~env ~pos:e.pos tp e, sch.sch_body, r_eff1)
            | None -> None
            end
          | _ -> None
          end 
        in
        match res with
        | Some(n, e, tp, r_eff1) -> (n, e, T.Scheme.of_type tp, r_eff1)
        | None -> 
          Error.warn (Error.redundant_named_parameter ~pos n);
          let (e, tp, r_eff1) = infer_expr_type env e eff in
          (n, e, T.Scheme.of_type tp, r_eff1)
    in
    let (ctx, insts, r_eff2) =
      check_explicit_insts ~tcfix env named insts cache eff in
    let x = Var.fresh () in
    let ctx e0 = make (T.ELet(x, sch, e, ctx e0)) in
    let insts = (n, make (T.EVar x)) :: insts in
    (ctx, insts, ret_effect_join r_eff1 r_eff2)
