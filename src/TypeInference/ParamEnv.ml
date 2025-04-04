(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Part of the environment that manages section parameters. *)

open Common

(** Usage state of declared parameters. Each declared parameter may be in one
  of four states:
  - *not generalized* (when not present in the parameter map of the
    environment) -- the parameter is in this state when we don't enter the
    scope where it might be used.
  - other state are described in [use_state] type *)
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

(** Parameter declaration *)
type param_decl =
  | ParamType of (** Named type parameter *)
    { pos : Position.t;
      (** Position of the declaration. *)

      name : T.tname;
      (** Name of the type parameter *)

      uid : UID.t;
      (** Unique identifier of the type parameter *)

      kind : T.kind;
      (** Kind of the type parameter *)
    }

  | ParamVal of (** Value parameter *)
    { pos        : Position.t;
      (** Position of the declaration. *)

      free_types : T.tvar list;
      (** List of type variables that are free in the scheme, and will be
        reinstantiated at each use. *)

      used_types : (UID.t * T.tvar) list;
      (** List of type parameters that are used in the scheme. The list
        contains pairs built from the following elements:
        - unique identifier of the type parameter (same as [uid] field of
          [ParamType] constructor),
        - the type variable used to represent this type parameter in the
          scheme. *)

      name       : Name.t;
      (** Name of the value parameter *)

      uid        : UID.t;
      (** Unique identifier of the value parameter *)

      scheme     : T.scheme_expr
      (** Scheme of the value parameter *)
    }

type ('name, 'a) param_status =
  | StNotGeneralized of 'name * Position.t
  | StValid          of 'a
  | StRejected       of 'name * Position.t

type 'name use =
  { u_name     : 'name;
    u_use_pos  : Position.t;
    u_decl_pos : Position.t
  }

type ('name, 'a) on_use = Position.t -> ('name, 'a) param_status

type t =
  { param_list : param_decl list;
    (** List of active parameter declarations (in reversed order). Already
      generalized declarations are not stored on this list. *)

    tvar_map   : (T.tname, T.tvar) on_use UID.Map.t;
    (** Map from type parameters to functions that checks the status of the
      parameter. *)

    val_map    : (Name.t, T.var * T.scheme) on_use UID.Map.t
    (** Map from value parameters to functions that checks the status of the
      parameter. *)
  }

type type_param = {
  tp_pos  : Position.t;
  tp_name : T.tname;
  tp_tvar : T.tvar;
  tp_uid  : UID.t;
  tp_used : use_state BRef.t
}

type val_param = {
  vp_pos    : Position.t;
  vp_name   : Name.t;
  vp_var    : T.var;
  vp_scheme : T.scheme_expr;
  vp_used   : use_state BRef.t
}

type param_list = {
  pl_scope : Scope.t;
    (** Parent scope of the parameter list. Unification variables at higher
      level can be generalized. *)

  pl_pp_tree : PPTree.t;
    (** pretty-printing information of extended environment *)

  pl_type_params : type_param list;
    (** List of type parameters that potentially could be generalized. *)

  pl_val_params  : val_param list
    (** List of value parameters that potentially could be generalized. *)
}

let empty =
  { param_list = [];
    tvar_map   = UID.Map.empty;
    val_map    = UID.Map.empty
  }

let scope params = params.pl_scope

let pp_tree params = params.pl_pp_tree

(* ========================================================================= *)

let on_use_type status decl_pos name x pos =
  match BRef.get status with
  | NotUsed  ->
    BRef.set status (Used pos);
    StValid x
  | Used _   -> StValid x
  | Rejected -> StRejected(name, decl_pos)

let on_use_val status decl_pos name x sch on_use_list pos =
  match BRef.get status with
  | NotUsed ->
    List.iter
      (fun on_use ->
        match on_use pos with
        | StValid _ -> ()
        | StNotGeneralized _ | StRejected _ ->
          (* If the value parameter is valid, then all used type parameters
            should be valid too. *)
          assert false)
      on_use_list;
    BRef.set status (Used pos);
    StValid (x, sch)
  | Used _  -> StValid (x, sch)
  | Rejected -> StRejected(name, decl_pos)

let open_param_decl ~new_scope (env, tps, vps, tmap) decl =
  match decl with
  | ParamType { pos; name; uid; kind } ->
    assert (not (UID.Map.mem uid env.tvar_map));
    let x = T.TVar.fresh ~pp_uid:(PP_UID uid) ~scope:new_scope kind in
    let status = BRef.create NotUsed in
    let on_use = on_use_type status pos name x in
    let env =
      { env with tvar_map = UID.Map.add uid on_use env.tvar_map } in
    let tmap = UID.Map.add uid (x, on_use) tmap in
    let param =
      { tp_pos  = pos;
        tp_name = name;
        tp_tvar = x;
        tp_uid  = uid;
        tp_used = status
      } in
    (env, param :: tps, vps, tmap)

  | ParamVal { pos; used_types; free_types; name; uid; scheme } ->
    assert (not (UID.Map.mem uid env.val_map));
    let status = BRef.create NotUsed in
    let (sub, on_use_list) =
      List.fold_left_map
        (fun sub (uid, x) ->
          let (y, on_use) = UID.Map.find uid tmap in
          let sub = T.Subst.add_type sub x (T.Type.t_var y) in
          (sub, on_use))
        (T.Subst.empty ~scope:new_scope)
        used_types in
    let sub =
      List.fold_left
        (fun sub x ->
          let tp = T.Type.fresh_uvar ~pos ~scope:new_scope (T.TVar.kind x) in
          T.Subst.add_type sub x tp)
        sub
        free_types
    in
    let scheme = T.SchemeExpr.subst sub scheme in
    let sch = T.SchemeExpr.to_scheme scheme in
    let x = Var.fresh ~name:(Name.to_string name) () in
    let on_use = on_use_val status pos name x sch on_use_list in
    let env =
      { env with val_map = UID.Map.add uid on_use env.val_map } in
    let param =
      { vp_pos    = pos;
        vp_name   = name;
        vp_var    = x;
        vp_scheme = scheme;
        vp_used   = status
      } in
    (env, tps, param :: vps, tmap)

let begin_generalize ~pp env scope =
  let new_scope = Scope.enter scope in
  let (env, tps, vps, _) =
    List.fold_left (open_param_decl ~new_scope)
      ({ env with param_list = [] }, [], [], UID.Map.empty)
      (List.rev env.param_list) in
  let params =
    { pl_scope       = scope;
      pl_pp_tree     = pp;
      pl_type_params = List.rev tps;
      pl_val_params  = List.rev vps
    } in
  (env, new_scope, params)

(* ========================================================================= *)

let select_type_param param =
  match BRef.get param.tp_used with
  | NotUsed  ->
    BRef.set param.tp_used Rejected;
    None
  | Used _   -> Some (param.tp_name, param.tp_uid, param.tp_tvar)
  | Rejected -> assert false

let select_val_param param =
  match BRef.get param.vp_used with
  | NotUsed  ->
    BRef.set param.vp_used Rejected;
    None
  | Used _   -> Some (param.vp_name, param.vp_var, param.vp_scheme)
  | Rejected -> assert false

let end_generalize_pure params  =
  let targs2 = List.filter_map select_type_param params.pl_type_params in
  let named  = List.filter_map select_val_param params.pl_val_params in
  (targs2, named)

(* ========================================================================= *)

let check_type_not_used param =
  let status = BRef.get param.tp_used in
  BRef.set param.tp_used Rejected;
  match status with
  | NotUsed      ->
    None
  | Used use_pos ->
    Some {
      u_name     = param.tp_name;
      u_use_pos  = use_pos;
      u_decl_pos = param.tp_pos
    }
  | Rejected ->
    (* Parameter cannot be rejected twice *)
    assert false

let check_val_not_used param =
  let status = BRef.get param.vp_used in
  BRef.set param.vp_used Rejected;
  match status with
  | NotUsed      -> None
  | Used use_pos ->
    Some {
      u_name     = param.vp_name;
      u_use_pos  = use_pos;
      u_decl_pos = param.vp_pos
    }
  | Rejected ->
    (* Parameter cannot be rejected twice *)
    assert false

let end_generalize_impure params =
  let used_types = List.filter_map check_type_not_used params.pl_type_params in
  let used_vals  = List.filter_map check_val_not_used  params.pl_val_params in
  (used_types, used_vals)

(* ========================================================================= *)

let declare_type ~pos env name uid kind =
  let decl = ParamType
    { pos  = pos;
      name = name;
      uid  = uid;
      kind = kind
    } in
  { env with
    param_list = decl :: env.param_list
  }

let declare_val ~pos env ~free_types ~used_types ~name uid scheme =
  let decl = ParamVal
    { pos        = pos;
      free_types = free_types;
      used_types = used_types;
      name       = name;
      uid        = uid;
      scheme     = scheme
    } in
  { env with
    param_list = decl :: env.param_list
  }

(* ========================================================================= *)

let find_type_param param_uid params =
  let func param =
    match param with
    | ParamType { pos; name; uid; _ } when UID.compare uid param_uid = 0 ->
      Some(name, pos)
    | ParamType _ | ParamVal _ -> None
  in
  match List.find_map func params with
  | Some(name, decl_pos) -> (name, decl_pos)
  | None ->
    (* It should not be possible to have a type parameter that is not int the
      list of parameters nor in the map. *)
    assert false

let find_val_param param_uid params =
  let func param =
    match param with
    | ParamVal { pos; name; uid; _ } when UID.compare uid param_uid = 0 ->
      Some(name, pos)
    | ParamType _ | ParamVal _ -> None
  in
  match List.find_map func params with
  | Some(name, decl_pos) -> (name, decl_pos)
  | None ->
    (* It should not be possible to have a value parameter that is not in the
      list of parameters nor in the map. *)
    assert false

let check_type_param ~pos env uid =
  match UID.Map.find_opt uid env.tvar_map with
  | None ->
    let (name, decl_pos) = find_type_param uid env.param_list in
    StNotGeneralized(name, decl_pos)

  | Some on_use -> on_use pos

let check_val_param ~pos env uid =
  match UID.Map.find_opt uid env.val_map with
  | None ->
    let (name, decl_pos) = find_val_param uid env.param_list in
    StNotGeneralized(name, decl_pos)

  | Some on_use -> on_use pos
