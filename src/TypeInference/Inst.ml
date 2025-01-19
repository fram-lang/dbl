(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-checking of explicit instantiations *)

open Common
open TypeCheckFix

module StrMap = Map.Make(String)

type type_name_status =
  | TProvided    of Position.t
  | TModProvided of Position.t
  | TNotProvided of T.kind

type val_name_status =
  | VProvided    of Position.t
  | VModProvided of Position.t
  | VNotProvided of T.scheme
  | VOptional    of T.typ

(* ========================================================================= *)
(** Create a map from names of type parameters to their status *)
let named_type_args sch_targs =
  sch_targs
  |> List.filter_map
      (fun (name, tvar) ->
        match name with
        | T.TNAnon  -> None
        | T.TNVar x -> Some (x, TNotProvided (T.TVar.kind tvar)))
  |> StrMap.of_list

(* ------------------------------------------------------------------------- *)
(** Preprocessed type parameters *)
type type_inst =
  | TI_Redundant of S.type_expr
    (** This parameter is redundant. It should be kind-checked, but it will
      not be used in the actual instantiation. *)

  | TI_Provided of S.tvar * S.type_expr * T.kind
    (** This parameter is provided. *)

  | TI_ModProvided of S.tvar * T.typ * T.kind * Position.t
    (** This parameter is provided by a module or the environment. *)

(** Preprocess type instantiations taken from a module or the environment. *)
let preprocess_module_type_insts ~pos lookup insts tnames =
  let preprocess name status (insts, tnames) =
    match status with
    | TProvided _ | TModProvided _ -> (insts, tnames)
    | TNotProvided kind ->
      begin match lookup name with
      | None -> (insts, tnames)
      | Some tp ->
        let tnames = StrMap.add name (TModProvided pos) tnames in
        (TI_ModProvided(name, tp, kind, pos) :: insts, tnames)
      end
  in
  StrMap.fold preprocess tnames (insts, tnames)

(** Preprocess type instantiations, i.e., decide which type parameters are
  provided. *)
let rec preprocess_type_insts env (insts : S.inst list) tnames =
  match insts with
  | [] -> ([], tnames)

  | { data = IType(name, tp); pos } :: insts ->
    let (insts, tnames) = preprocess_type_insts env insts tnames in
    begin match StrMap.find_opt name tnames with
    | None ->
      Error.warn (Error.redundant_named_type ~pos name);
      (TI_Redundant tp :: insts, tnames)

    | Some (TNotProvided kind) ->
      let tnames = StrMap.add name (TProvided pos) tnames in
      (TI_Provided(name, tp, kind) :: insts, tnames)

    | Some (TProvided npos | TModProvided npos) ->
      Error.report (Error.type_already_provided ~pos ~npos name);
      (TI_Redundant tp :: insts, tnames)
    end

  | { data = IVal _; pos } :: insts ->
    preprocess_type_insts env insts tnames

  | { data = IOpen; pos } :: insts ->
    let (insts, tnames) = preprocess_type_insts env insts tnames in
    let lookup = ModulePath.try_lookup_tvar ~pos env in
    preprocess_module_type_insts ~pos lookup insts tnames

  | { data = IModule path; pos } :: insts ->
    let (insts, tnames) = preprocess_type_insts env insts tnames in
    let modl = ModulePath.lookup_module env path in
    let lookup = ModulePath.try_lookup_tvar_in_module ~pos env modl in
    preprocess_module_type_insts ~pos lookup insts tnames

(* ------------------------------------------------------------------------- *)
(** Check the kind of a single preprocessed type parameter *)
let check_kind_of_type_param env inst =
  match inst with
  | TI_Redundant tp ->
    let _ = Type.infer_kind env tp in
    None

  | TI_Provided(name, tp, kind) ->
    let tp = Type.check_kind env tp kind in
    Some (name, tp)

  | TI_ModProvided(name, tp, kind, pos) ->
    let tp_kind = T.Type.kind tp in
    if not (Unification.unify_kind tp_kind kind) then
      Error.report
        (Error.named_type_kind_mismatch ~pos name tp_kind kind);
    Some (name, { T.pos; T.data = T.TE_Type tp })

(** Check kinds of all preprocessed type parameters in the order that appears
  in the source. *)
let check_kinds_of_type_params env insts =
  List.filter_map (check_kind_of_type_param env) insts
  |> StrMap.of_list

(* ------------------------------------------------------------------------- *)
let build_type_params ~pos ~env insts sch_targs =
  let build_type_param sub (name, x) =
    match name with
    | T.TNAnon ->
      let tp = Env.fresh_uvar env (T.TVar.kind x) in
      let tp_expr = { T.pos; T.data = T.TE_Type tp } in
      (T.Subst.add_type sub x tp, tp_expr)

    | T.TNVar name ->
      begin match StrMap.find_opt name insts with
      | None ->
        let tp = Env.fresh_uvar env (T.TVar.kind x) in
        let tp_expr = { T.pos; T.data = T.TE_Type tp } in
        (T.Subst.add_type sub x tp, tp_expr)

      | Some tp ->
        (T.Subst.add_type sub x (T.TypeExpr.to_type tp), tp)
      end
  in
  List.fold_left_map build_type_param T.Subst.empty sch_targs

(* ------------------------------------------------------------------------- *)
let check_type_params ~pos env insts sch_targs =
  let tnames = named_type_args sch_targs in
  let (insts, _) = preprocess_type_insts env insts tnames in
  let insts = check_kinds_of_type_params env insts in
  build_type_params ~pos ~env insts sch_targs

(* ========================================================================= *)
(* TODO: explicit method instantiation is not allowed yet, because it is more
  complex than it seems. We have to be able to discover ambiguous
  instantiations like the following.
  ```
    let foo {T, U, method m : T -> T, method m : U -> U} = ...

    ... foo {method m = fn x => x} ...
    ... foo {T = Int, method m = fn (x : Int) => x} ...
    ... foo {T = Int, U = Int, method m = fn (x : Int) => x} ...
  ```
  Moreover, explicit method instantiations interact with module instantiations
  in a way that is not yet clear, for example:
  ```
    module M
      pub method m = ...
    end
    ... foo { method m = ..., module M } ...
  ```
  Should the module shadow the method instantiation or not or should it depend
  on types? *)

(* Create an initial status of named parameters. *)
let named_args ~pos ~pp sch_named =
  let add_named_arg status (name, sch) =
    match name with
    | T.NVar x ->
      Name.Map.add (NVar x) (VNotProvided sch) status
    | T.NOptionalVar x ->
      let tp = BuiltinTypes.scheme_to_option_arg sch in
      Name.Map.add (NOptionalVar x) (VOptional tp) status
    | T.NImplicit x ->
      Name.Map.add (NImplicit x) (VNotProvided sch) status
    | T.NMethod m ->
      let owner = NameUtils.method_owner_of_scheme ~pos ~pp sch in
      Name.Map.add (NMethod(owner, m)) (VNotProvided sch) status
  in
  List.fold_left add_named_arg Name.Map.empty sch_named

(* ------------------------------------------------------------------------- *)
(** Preprocessed value instantiations *)
type value_inst =
  | VI_Redundant of S.poly_expr_def
    (** This parameter is redundant. It should be type-checked, but it will
      not be used in the actual instantiation. *)

  | VI_Provided of Name.t * S.poly_expr_def * T.scheme
    (** This parameter is provided and should have a given scheme.
      It is also used to provide "optional as Option" parameters, i.e.,
      [{?x = ...}]. *)

  | VI_OptProvided of Name.t * S.poly_expr_def * T.typ
    (** This optional parameter is provided and should have a given type. *)

  | VI_ModProvided of Name.t * T.poly_expr * T.scheme * T.scheme * Position.t
    (** This parameter is provided by a module or the environment. The first
      scheme is an actual scheme, and the second one is a scheme that is
      expected. *)

  | VI_OptModProvided of S.var * T.poly_expr * T.scheme * T.typ * Position.t
    (** This optional parameter is provided by a module or the environment.
      It stores the name of the parameter, the actual expression, the actual
      scheme, the expected type, and the position of the instantiation. *)

(** Preprocess value instantiations taken from a module or the environment. *)
let preprocess_module_val_insts ~pos ~lookup insts names =
  let preprocess_val (name : Name.t) status (insts, names) =
    match name, status with
    | _, (VProvided _ | VModProvided _) ->
      (insts, names)

    | (NVar _ | NImplicit _ | NMethod _), VNotProvided sch ->
      begin match lookup name with
      | None -> (insts, names)

      | Some(poly_expr, poly_sch) ->
        let names = Name.Map.add name (VModProvided pos) names in
        let insts =
          VI_ModProvided(name, poly_expr, poly_sch, sch, pos) :: insts in
        (insts, names)
      end

    | NOptionalVar opt_name, VOptional tp ->
      begin match lookup name with
      | None -> (insts, names)

      | Some(poly_expr, poly_sch) ->
        let names = Name.Map.add name (VModProvided pos) names in
        let insts =
          VI_OptModProvided(opt_name, poly_expr, poly_sch, tp, pos) :: insts in
        (insts, names)
      end

    | (NVar _ | NImplicit _ | NMethod _), VOptional _ ->
      (* [VOptional] status is possible only for optional parameters *)
      assert false
    | NOptionalVar _, VNotProvided _ ->
      (* Optional parameters are treated in a special way. They use [VOptional]
        constructor. *)
      assert false
  in
  (insts, names)
  |> Name.Map.fold preprocess_val names

(** Preprocess value instantiations, i.e., decide which named parameters are
  provided. *)
let rec preprocess_val_insts env (insts : S.inst list) names =
  let pp = Env.pp_tree env in
  match insts with
  | [] -> ([], names)

  | { data = IType _; pos } :: insts -> preprocess_val_insts env insts names

  | { data = IVal((NVar _ | NImplicit _) as name, e); pos } :: insts ->
    let name =
      match name with
      | NVar x      -> Name.NVar x
      | NImplicit x -> Name.NImplicit x
      | _ -> assert false
    in
    let (insts, names) = preprocess_val_insts env insts names in
    begin match Name.Map.find_opt name names with
    | None ->
      Error.warn (Error.redundant_named_parameter ~pos ~pp name);
      (VI_Redundant e :: insts, names)

    | Some (VNotProvided sch) ->
      let names = Name.Map.add name (VProvided pos) names in
      (VI_Provided(name, e, sch) :: insts, names)

    | Some (VOptional tp) ->
      let names = Name.Map.add name (VProvided pos) names in
      (VI_OptProvided(name, e, tp) :: insts, names)

    | Some (VProvided npos | VModProvided npos) ->
      (* extract exact name, in order to distinguish between regular and
        optional parameters *)
      let name = fst (Name.Map.find_first (Name.equal name) names) in
      Error.report (Error.named_param_already_provided ~pos ~npos ~pp name);
      (VI_Redundant e :: insts, names)
    end

  | { data = IVal(NOptionalVar x, e); pos } :: insts ->
    let (insts, names) = preprocess_val_insts env insts names in
    let name = Name.NOptionalVar x in
    begin match Name.Map.find_opt name names with
    | None ->
      Error.warn (Error.redundant_named_parameter ~pos ~pp name);
      (VI_Redundant e :: insts, names)

    | Some (VOptional tp) ->
      let sch = BuiltinTypes.mk_option_scheme tp in
      let names = Name.Map.add name (VProvided pos) names in
      (VI_Provided(name, e, sch) :: insts, names)

    | Some (VProvided npos | VModProvided npos) ->
      Error.report (Error.named_param_already_provided ~pos ~npos ~pp name);
      (VI_Redundant e :: insts, names)

    | Some (VNotProvided _) ->
      Error.report (Error.named_param_provided_as_optional ~pos x);
      (VI_Redundant e :: insts, names)
    end

  | { data = IVal (NMethod _, e); pos } :: insts ->
    (* TODO; not implemented. See the comment above. *)
    Error.report (Error.method_instantiation_not_allowed ~pos);
    let (insts, names) = preprocess_val_insts env insts names in
    (VI_Redundant e :: insts, names)

  | { data = IOpen; pos } :: insts ->
    let (insts, names) = preprocess_val_insts env insts names in
    preprocess_module_val_insts ~pos
      ~lookup:(ModulePath.try_lookup_val ~pos env)
      insts names

  | { data = IModule path; pos } :: insts ->
    let (insts, names) = preprocess_val_insts env insts names in
    let modl = ModulePath.lookup_module env path in
    preprocess_module_val_insts ~pos
      ~lookup:(ModulePath.try_lookup_val_in_module ~pos env modl)
      insts names

(* ------------------------------------------------------------------------- *)

(** Create a context with polymorphic let-definition. *)
let let_poly_ctx e cs =
  let x = Var.fresh () in
  let ctx er =
    { er_expr   = make_nowhere (T.ELetPoly(x, e, er.er_expr));
      er_type   = er.er_type;
      er_effect = er.er_effect;
      er_constr = cs @ er.er_constr;
    } in
  (x, ctx)

(** Create a context with monomorphic let-definition. *)
let let_mono_ctx er1 =
  let x = Var.fresh () in
  let ctx er2 =
    { er_expr   = make_nowhere (T.ELetMono(x, er1.er_expr, er2.er_expr));
      er_type   = er2.er_type;
      er_effect = T.Effect.join er1.er_effect er2.er_effect;
      er_constr = er1.er_constr @ er2.er_constr;
    } in
  (x, ctx)

(** Create a context that adds constraints. *)
let add_constr_ctx cs er =
  { er with er_constr = cs @ er.er_constr }

let default_check_provided ~tcfix ~wrap env e sch =
  match PolyExpr.check_def_scheme ~tcfix env e sch with
  | Mono er ->
    let (x_var, i_ctx) = let_mono_ctx er in
    (i_ctx, wrap { T.pos = e.pos; T.data = T.EVar x_var })

  | Poly(e, cs) ->
    (add_constr_ctx cs, wrap e)

(** Check the type of a single preprocessed value parameter *)
let check_type_of_val_param ~tcfix env inst val_insts =
  match inst with
  | VI_Redundant e ->
    begin match PolyExpr.infer_def_scheme ~tcfix env e with
    | PPure(e, _, cs) ->
      let (_, i_ctx) = let_poly_ctx e cs in
      (i_ctx, val_insts)
    | PImpure er ->
      let (_, i_ctx) = let_mono_ctx er in
      (i_ctx, val_insts)
    end

  | VI_Provided(name, e, sch) ->
    let (i_ctx, e) =
      default_check_provided ~tcfix ~wrap:Fun.id env e sch in
    let val_insts = Name.Map.add name e val_insts in
    (i_ctx, val_insts)

  | VI_OptProvided(x, e, tp) ->
    let make data = { T.pos = e.pos; T.data } in
    let (i_ctx, e) =
      default_check_provided ~tcfix
        ~wrap:(fun e ->
          make (T.EPolyFun([], [],
            BuiltinTypes.mk_some_poly ~pos:e.pos tp e)))
      env e (T.Scheme.of_type tp) in
    let val_insts = Name.Map.add x e val_insts in
    (i_ctx, val_insts)

  | VI_ModProvided(name, e, poly_sch, sch, pos) ->
    let (e, cs) = ParamResolve.coerce_scheme ~pos ~name env e poly_sch sch in
    let val_insts = Name.Map.add name e val_insts in
    (add_constr_ctx cs, val_insts)

  | VI_OptModProvided(x, e, poly_sch, tp, pos) ->
    let (e, cs) = ParamResolve.coerce_scheme ~pos ~name:(NOptionalVar x)
      env e poly_sch (T.Scheme.of_type tp) in
    let poly_expr =
      { T.pos = pos;
        T.data = (T.EPolyFun([], [], BuiltinTypes.mk_some_poly ~pos tp e))
      } in
    let val_insts = Name.Map.add (NOptionalVar x) poly_expr val_insts in
    (add_constr_ctx cs, val_insts)

(** Check types of all preprocessed value parameters in the order that appears
  in the source. *)
let check_types_of_val_params ~tcfix env insts =
  let check (i_ctx1, val_insts) inst =
    let (i_ctx2, val_insts) =
      check_type_of_val_param ~tcfix env inst val_insts in
    (Fun.compose i_ctx1 i_ctx2, val_insts)
  in
  List.fold_left check (Fun.id, Name.Map.empty) insts

(* ------------------------------------------------------------------------- *)
(** Build a final list of named parameters *)
let build_named_params ~pos env insts sch_named =
  let make data = { T.pos; T.data } in
  let build_named_param (name, sch) =
    match name with
    | T.NVar x ->
      begin match Name.Map.find_opt (NVar x) insts with
      | Some e -> (e, [])
      | None -> Error.fatal (Error.cannot_resolve_named_param ~pos x)
      end

    | T.NOptionalVar x ->
      begin match Name.Map.find_opt (NOptionalVar x) insts with
      | Some e -> (e, [])
      | None ->
        let e =
          BuiltinTypes.mk_none ~pos (BuiltinTypes.scheme_to_option_arg sch) in
        (make (T.EPolyFun([], [], e)), [])
      end

    | T.NImplicit iname ->
      begin match Name.Map.find_opt (NImplicit iname) insts with
      | Some e -> (e, [])
      | None   -> ParamResolve.resolve_implicit ~pos env iname sch
      end

    | T.NMethod mname ->
      (* TODO: not implemented. See the comment above. *)
      ParamResolve.resolve_method ~pos env mname sch
  in
  List.fold_left_map
    (fun cs1 param ->
      let (param, cs2) = build_named_param param in
      (cs1 @ cs2, param))
    []
    sch_named

(* ------------------------------------------------------------------------- *)
let check_named_params ~tcfix ~pos env insts sch_named =
  let names = named_args ~pos ~pp:(Env.pp_tree env) sch_named in
  let (insts, _) = preprocess_val_insts env insts names in
  let (i_ctx, insts) = check_types_of_val_params ~tcfix env insts in
  let (cs, params) = build_named_params ~pos env insts sch_named in
  (Fun.compose i_ctx (add_constr_ctx cs), params)

(* ========================================================================= *)
let instantiate_poly_expr ~tcfix ~pos env e (sch : T.scheme) inst =
  let (sub, tps) = check_type_params ~pos env inst sch.sch_targs in
  let named_schemes = List.map (T.NamedScheme.subst sub) sch.sch_named in
  let (i_ctx, named_args) =
    check_named_params ~tcfix ~pos env inst named_schemes in
  let res =
    { er_expr   = { pos; data = T.EInst(e, tps, named_args) };
      er_type   = Infered (T.Type.subst sub sch.sch_body);
      er_effect = Pure;
      er_constr = []
    } in
  i_ctx res
