(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Additional environment used in type-checking definition blocks. It stores
  information about declared parameters. *)

open Common

(** Parameter declaration *)
type param_decl =
  | ParamType of (** Named type parameter *)
    { pos : Position.t;
      (** Position of the declaration. *)

      name : T.tname;
      (** Name of the type parameter that will be visible in the type scheme.
        *)

      local_name : T.tname;
      (** Local name of the parameter. It is used in the type environment. *)

      tvar : T.tvar;
      (** Type variable that represents the parameter. *)
    }

  | ParamVal of (** Value parameter *)
    { pos : Position.t;
      (** Position of the declaration. *)

      name : T.name;
      (** External name of the parameter. *)

      ident : S.ident;
      (** Internal identifier of the parameter. *)

      used_types : T.tvar list;
      (** List of type parameters that are used in the parameter. *)

      free_types : T.tvar list;
      (** List of type variables in which different instances of this
        parameter may differ. *)

      scheme : T.scheme_expr;
      (** Scheme of the parameter. *)
    }

type t = param_decl list (* in reversed order *)

(* ========================================================================= *)

(** Usage state of named parameter *)
type use_state =
  | NotUsed
    (** Parameter was not used yet. This state may change in the future, but
      if not, the parameter will not be generalized *)

  | Used of Position.t
    (** Parameter was used at given position. It will be generalized, or the
      error will be raised if the generalization is not possible *)

  | Rejected
    (** The parameter was not used and it will not be generalized. If a
      constraint solving tries to use this parameter, an error is raised. *)

type type_param = {
  tp_pos  : Position.t;
  tp_name : T.tname;
  tp_tvar : T.tvar;
  tp_used : use_state BRef.t
}

type val_param = {
  vp_pos    : Position.t;
  vp_name   : T.name;
  vp_var    : T.var;
  vp_scheme : T.scheme_expr;
  vp_used   : use_state BRef.t
}

type param_list = {
  pl_level : int;
    (** Level of the parent environment. Unification variables with the
      higher level can be generalized. *)

  pl_env_ext : Env.t;
    (** Environment extended with declared parameters (for printing error
      messages). *)
  
  pl_type_params : type_param list;
    (** List of type parameters that potentially could be generalized. *)

  pl_val_params  : val_param list
    (** List of value parameters that potentially could be generalized. *)
}

let empty = []

(* ========================================================================= *)

let on_use_type status def_pos name pos =
  match BRef.get status with
  | NotUsed -> BRef.set status (Used pos)
  | Used _  -> ()
  | Rejected ->
    Error.fatal (Error.rejected_type_param_used ~pos ~def_pos name)

let on_use_val status def_pos name on_use_list pos =
  match BRef.get status with
  | NotUsed ->
    List.iter (fun on_use -> on_use pos) on_use_list;
    BRef.set status (Used pos)
  | Used _  -> ()
  | Rejected ->
    Error.fatal (Error.rejected_named_param_used ~pos ~def_pos name)

let open_param_decl (env, tps, vps, tmap) decl =
  match decl with
  | ParamType { pos; name; local_name; tvar } ->
    let status = BRef.create NotUsed in
    let on_use = on_use_type status pos name in
    let env =
      match local_name with
      | TNAnon ->
        let name =
          match name with
          | TNAnon -> None
          | TNVar x -> Some x
        in
        Env.add_existing_anon_tvar env ~pos ?name tvar
      | TNVar x ->
        Env.add_existing_tvar ~pos ~on_use env x tvar
    in
    let tmap = T.TVar.Map.add tvar on_use tmap in
    let param =
      { tp_pos  = pos;
        tp_name = name;
        tp_tvar = tvar;
        tp_used = status
      } in
    (env, param :: tps, vps, tmap)

  | ParamVal { pos; name; ident; used_types; free_types; scheme } ->
    let status = BRef.create NotUsed in
    let on_use_list = List.map (fun x -> T.TVar.Map.find x tmap) used_types in
    let sub =
      List.fold_left
        (fun sub x ->
          let tp = Env.fresh_uvar env (T.TVar.kind x) in
          T.Subst.add_type sub x tp)
        T.Subst.empty
        free_types
    in
    let scheme = T.SchemeExpr.subst sub scheme in
    let sch = T.SchemeExpr.to_scheme scheme in
    let on_use = on_use_val status pos name on_use_list in
    let (env, x) =
      match ident with
      | IdVar x ->
        Env.add_var ~on_use env x sch
      | IdImplicit x ->
        Env.add_implicit ~on_use env x sch
      | IdMethod name ->
        let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
        Env.add_method ~on_use env owner name sch
    in
    let param =
      { vp_pos    = pos;
        vp_name   = name;
        vp_var    = x;
        vp_scheme = scheme;
        vp_used   = status
      } in
    (env, tps, param :: vps, tmap)

let open_param_decls env penv =
  let (env, tps, vps, _) =
    List.fold_left open_param_decl
      (env, [], [], T.TVar.Map.empty)
      (List.rev penv) in
  (env, List.rev tps, List.rev vps)

let begin_generalize env penv =
  let level = Env.level env in
  let (env, tps, vps) = open_param_decls (Env.incr_level env) penv in
  let params =
    { pl_level       = level;
      pl_env_ext     = env;
      pl_type_params = tps;
      pl_val_params  = vps
    } in
  (env, params)

(* ========================================================================= *)

let select_type_param param =
  match BRef.get param.tp_used with
  | NotUsed  ->
    BRef.set param.tp_used Rejected;
    None
  | Used _   -> Some (param.tp_name, param.tp_tvar)
  | Rejected -> assert false

let select_named_param param =
  match BRef.get param.vp_used with
  | NotUsed  ->
    BRef.set param.vp_used Rejected;
    None
  | Used _   -> Some (param.vp_name, param.vp_var, param.vp_scheme)
  | Rejected -> assert false

let end_generalize_pure params uvs cs =
  let cs = Constr.solve_partial cs in
  (* Collect all used parameters *)
  let targs2 = List.filter_map select_type_param params.pl_type_params in
  let named  = List.filter_map select_named_param params.pl_val_params in
  (* Collect all unification variables from generalized named parameters *)
  let uvs =
    List.fold_left
      (fun uvs (_, _, sch) ->
        T.Scheme.collect_uvars (T.SchemeExpr.to_scheme sch) uvs)
      uvs named in
  (* Generalize all unification variables *)
  let targs1 =
    T.UVar.Set.filter (fun x -> T.UVar.level x > params.pl_level) uvs
    |> T.UVar.Set.elements
    |> List.map (fun x -> (T.TNAnon, T.UVar.fix x)) in
  (* Fix scopes of constraints *)
  let new_tvars = List.map snd targs1 |> T.TVar.Set.of_list in
  let cs = Constr.fix_scopes new_tvars cs in
  (targs1 @ targs2, named, cs)

(* ========================================================================= *)

let check_type_not_used param =
  begin match BRef.get param.tp_used with
  | NotUsed  -> ()
  | Used pos ->
    Error.report
      (Error.ungeneralizable_type_param ~pos ~def_pos:param.tp_pos
        param.tp_name)
  | Rejected -> assert false
  end;
  BRef.set param.tp_used Rejected

let check_named_not_used param =
  begin match BRef.get param.vp_used with
  | NotUsed  -> ()
  | Used pos ->
    Error.report
      (Error.ungeneralizable_named_param ~pos ~def_pos:param.vp_pos
        param.vp_name)
  | Rejected -> assert false
  end;
  BRef.set param.vp_used Rejected

let end_generalize_impure params uvs =
  T.UVar.Set.iter
    (fun u -> T.UVar.filter_scope u params.pl_level (fun _ -> true))
    uvs;
  List.iter check_type_not_used params.pl_type_params;
  List.iter check_named_not_used params.pl_val_params

(* ========================================================================= *)

let shadow_val penv (id : S.ident) =
  match id with
  | IdVar _ | IdImplicit _ ->
    penv |> List.filter (fun decl ->
      match decl with
      | ParamType _ -> true
      | ParamVal { ident; _ } -> ident <> id)
  | IdMethod _ ->
    (* Only methods of declared types can be declared, so there is no need to
       shadow them *)
    penv

let shadow_var      penv x = shadow_val penv (IdVar x)
let shadow_implicit penv x = shadow_val penv (IdImplicit x)

let shadow_vars = List.fold_left shadow_var
let shadow_implicits = List.fold_left shadow_implicit

let select_used_types param =
  match BRef.get param.tp_used with
  | NotUsed  -> None
  | Used _   -> Some param.tp_tvar
  | Rejected -> assert false

let end_generalize_declare ~pos params penv (name : S.name) id sch_expr =
  let used_types = List.filter_map select_used_types params.pl_type_params in
  let sch = T.SchemeExpr.to_scheme sch_expr in
  let free_types =
    T.Scheme.uvars sch
    |> T.UVar.Set.elements
    |> List.map (fun x -> T.UVar.fix x)
  in
  (* Check well-formedness of the scheme *)
  let (name, sch_expr) =
    begin match name with
    | NVar x -> (T.NVar x, sch_expr)
    | NOptionalVar x ->
      begin match T.SchemeExpr.to_type_expr sch_expr with
      | Some tp -> (T.NOptionalVar x, BuiltinTypes.mk_option_scheme_expr tp)
      | None ->
        Error.fatal (Error.polymorphic_optional_parameter ~pos)
      end
    | NImplicit x -> (T.NImplicit x, sch_expr)
    | NMethod name ->
      let owner =
        TypeUtils.method_owner_of_scheme ~pos ~env:params.pl_env_ext sch in
      if not (List.exists (T.TVar.equal owner) used_types) then
        Error.report (Error.method_owner_not_declared ~pos);
      (T.NMethod name, sch_expr)
    end
  in
  let decl =
    ParamVal {
      pos        = pos;
      name       = name;
      ident      = id;
      used_types = used_types;
      free_types = free_types;
      scheme     = sch_expr
    } in
  decl :: shadow_val penv id

(* ========================================================================= *)

let shadow_type penv (name : T.tname) =
  match name with
  | TNAnon -> penv
  | TNVar x ->
    penv |> List.map (fun decl ->
      match decl with
      | ParamType p when p.local_name = name ->
        ParamType { p with local_name = TNAnon }

      | ParamType _ | ParamVal _ -> decl)

let shadow_type_name penv x =
  shadow_type penv (TNVar x)

let shadow_type_names = List.fold_left shadow_type_name

let declare_type ~pos penv name x kind =
  let tvar = T.TVar.fresh kind in
  let decl =
    ParamType { pos; name = tr_tname name; local_name = TNVar x; tvar } in
  decl :: shadow_type penv (TNVar x)

(* ========================================================================= *)
let add_tvar ~pos ~public env penv name kind =
  let (env, x) = Env.add_tvar ~pos ~public env name kind in
  let penv = shadow_type penv (TNVar name) in
  (env, penv, x)

let add_poly_id ~pos ~public env penv (id : S.ident) sch =
  let penv = shadow_val penv id in
  match id with
  | IdVar x ->
    let (env, x) = Env.add_var env ~public x sch in
    (env, penv, x)

  | IdImplicit x ->
    let (env, x) = Env.add_implicit env ~public x sch in
    (env, penv, x)

  | IdMethod name ->
    let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
    let (env, x) = Env.add_method env ~public owner name sch in
    (env, penv, x)

let add_mono_id ~pos ~public env penv (id : S.ident) tp =
  add_poly_id ~pos ~public env penv id (T.Scheme.of_type tp)

let add_method_fn ~public env penv x name =
  let penv = shadow_val penv (IdVar name) in
  let env = Env.add_method_fn env ~public x name in
  (env, penv)

let add_ctor ~public env penv name idx adt =
  let penv = shadow_val penv (IdVar name) in
  let env = Env.add_ctor env ~public name idx adt in
  (env, penv)

let open_module ~public env penv m =
  let penv = shadow_type_names penv (Module.public_types m) in
  let penv = shadow_vars penv (Module.public_vars m) in
  let penv = shadow_implicits penv (Module.public_implicits m) in
  (* There is no need to shadow methods *)
  let env = Env.open_module ~public env m in
  (env, penv)

let add_partial_env env penv pat_env =
  let penv = shadow_type_names penv (PartialEnv.type_names pat_env) in
  let penv = shadow_vars penv (PartialEnv.var_names pat_env) in
  let penv = shadow_implicits penv (PartialEnv.implicit_names pat_env) in
  (* There is no need to shadow methods *)
  let env = PartialEnv.extend env pat_env in
  (env, penv)
