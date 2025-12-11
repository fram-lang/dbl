(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Partial environments created by patterns. *)

open Common

module StrMap = Map.Make(String)

type tvar_info = {
  ti_public : bool;
    (** The flag indicating whether the type variable is public. *)

  ti_pos    : Position.t;
    (** The position where the type variable is bound. *)
    
  ti_tvar   : T.tvar;
    (** The Unif representation of this type variable. *)
}

type var_info = {
  vi_public : bool;
    (** The flag indicating whether the variable is public. *)

  vi_pos    : Position.t;
    (** The position where the variable is bound. *)

  vi_var    : T.var;
    (** The Unif representation of this variable. *)

  vi_scheme : T.scheme;
    (** The scheme of the variable. *)
}

type module_info = {
  mi_public    : bool;
    (** The flag indicating whether the module is public. *)

  mi_pos       : Position.t;
    (** The position where the module is bound. *)

  mi_types     : (S.tvar * T.tvar) list;
    (** The public type variables of the module. *)
  
  mi_vals      : (Name.t * T.var * T.scheme) list;
    (** The public values of the module. *)
}

type is_anon = bool

type t = {
  tvar_map : tvar_info StrMap.t;
    (** Map from type variable names to type variable information. *)

  tvar_tab : (is_anon * Position.t) T.TVar.Map.t;
    (** Unif type variables bound by the pattern. It stores the place of
      binding. *)

  val_map  : var_info Name.Map.t;
    (** Map from names to variable information. *)

  module_map : module_info StrMap.t;
    (** Map from module names to module information. *)
}

let empty =
  { tvar_map     = StrMap.empty;
    tvar_tab     = T.TVar.Map.empty;
    val_map      = Name.Map.empty;
    module_map   = StrMap.empty
  }

let singleton_tvar_alias ~public ~pos name x =
  let info =
    { ti_public = public;
      ti_pos    = pos;
      ti_tvar   = x
    } in
  { empty with
    tvar_map = StrMap.singleton name info
  }

let singleton_val ~public ~pos name x sch =
  let info =
    { vi_public = public;
      vi_pos    = pos;
      vi_var    = x;
      vi_scheme = sch
    } in
  { empty with val_map = Name.Map.singleton name info }

let singleton_module ~public ~pos ~types ~vals modname =
  let info =
    { mi_public    = public;
      mi_pos       = pos;
      mi_types     = types;
      mi_vals      = vals
    } in
  { empty with
    module_map = StrMap.singleton modname info
  }

let add_anon_tvar ~pos penv x =
  assert (not (T.TVar.Map.mem x penv.tvar_tab));
  { penv with tvar_tab = T.TVar.Map.add x (true, pos) penv.tvar_tab }

let update_position tab x pos =
  match T.TVar.Map.find_opt x tab with
  | None -> tab
  | Some (is_anon, _) ->
    if is_anon then
      T.TVar.Map.add x (false, pos) tab
    else
      tab

let add_tvar_alias ~public ~pos penv name x =
  match StrMap.find_opt name penv.tvar_map with
  | None ->
    let info =
      { ti_public = public;
        ti_pos    = pos;
        ti_tvar   = x
      } in
    { penv with
      tvar_map = StrMap.add name info penv.tvar_map;
      tvar_tab = update_position penv.tvar_tab x pos
    }

  | Some info ->
    Error.report
      (Error.duplicate_type_in_pattern ~pos ~ppos:info.ti_pos name);
    penv

let add_val ~public ~pos ~pp penv name x sch =
  match Name.Map.find_opt name penv.val_map with
  | None ->
    let info =
      { vi_public = public;
        vi_pos    = pos;
        vi_var    = x;
        vi_scheme = sch
      } in
    { penv with val_map = Name.Map.add name info penv.val_map }

  | Some info ->
    Error.report
      (Error.duplicate_val_in_pattern ~pos ~ppos:info.vi_pos ~pp name);
    penv

let add_var ~public ~pos ~pp penv name x sch =
  add_val ~public ~pos ~pp penv (NVar name) x sch

let add_implicit ~public ~pos ~pp  penv name x sch =
  add_val ~public ~pos ~pp penv (NImplicit name) x sch

let add_method ~public ~pos ~pp penv owner name x sch =
  add_val ~public ~pos ~pp penv (NMethod(owner, name)) x sch

(* ========================================================================= *)

let join_tvar_info name info1 info2 =
  match info1, info2 with
  | None,       None       -> None
  | Some info,  None       -> Some info
  | None,       Some info  -> Some info
  | Some info1, Some info2 ->
    Error.report
      (Error.duplicate_type_in_pattern
        ~pos:info1.ti_pos ~ppos:info2.ti_pos name);
    Some info2

let join_tvar_pos _ p1 p2 =
  match p1, p2 with
  | None,    None -> None
  | Some p,  None -> Some p
  | None,    Some p -> Some p
  | Some p1, Some p2 -> assert false

let join_val_info ~pp name info1 info2 =
  match info1, info2 with
  | None,       None       -> None
  | Some info,  None       -> Some info
  | None,       Some info  -> Some info
  | Some info1, Some info2 ->
    Error.report
      (Error.duplicate_val_in_pattern ~pp
        ~pos:info1.vi_pos ~ppos:info2.vi_pos name);
    Some info2

let join_module_info name info1 info2 =
  match info1, info2 with
  | None,       None       -> None
  | Some info,  None       -> Some info
  | None,       Some info  -> Some info
  | Some info1, Some info2 ->
    Error.report
      (Error.duplicate_module_in_pattern
        ~pos:info1.mi_pos ~ppos:info2.mi_pos name);
    Some info2

let join ~pp penv1 penv2 =
  { tvar_map     =
      StrMap.merge join_tvar_info penv1.tvar_map penv2.tvar_map;
    tvar_tab     =
      T.TVar.Map.merge join_tvar_pos penv1.tvar_tab penv2.tvar_tab;
    val_map      =
      Name.Map.merge (join_val_info ~pp) penv1.val_map penv2.val_map;
    module_map   =
      StrMap.merge join_module_info penv1.module_map penv2.module_map
  }

(* ========================================================================= *)

let merge_opts ~on_mismatch ~on_both opt1 opt2 =
  match opt1, opt2 with
  | None, None -> None
  | Some _, None | None, Some _ -> on_mismatch (); None
  | Some v1, Some v2 -> on_both v1 v2

let intersect_tvar_info ~pos name info1 info2 =
  merge_opts info1 info2
    ~on_mismatch:(fun () -> Error.report (Error.or_pattern_type_vars_mismatch ~pos name))
    ~on_both:(fun info1 info2 ->
      if not (T.TVar.equal info1.ti_tvar info2.ti_tvar) then
        Error.report (Error.or_pattern_type_vars_mismatch ~pos name);
      Some info1)

let intersect_tvar_pos ~pos _ p1 p2 =
  merge_opts p1 p2
    ~on_mismatch:(fun () -> ())
    ~on_both:(fun p1 _ -> Some p1)

let intersect_val_info ~check_schemes ~pp ~pos name info1 info2 =
  merge_opts info1 info2
    ~on_mismatch:(fun () -> Error.report (Error.or_pattern_vars_mismatch ~pos ~pp name))
    ~on_both:(fun info1 info2 ->
      Option.iter (fun check -> check info1.vi_scheme info2.vi_scheme) check_schemes;
      Some info1)

let intersect_module_info ~check_schemes ~pp ~pos name info1 info2 =
  merge_opts info1 info2
    ~on_mismatch:(fun () -> Error.report (Error.or_pattern_modules_mismatch ~pos name))
    ~on_both:(fun info1 info2 ->
      (* Check types *)
      let types1 = StrMap.of_list info1.mi_types in
      let types2 = StrMap.of_list info2.mi_types in
      let check_type t_name t1 t2 =
        merge_opts t1 t2
          ~on_mismatch:(fun () -> Error.report (Error.or_pattern_type_vars_mismatch ~pos t_name))
          ~on_both:(fun t1 t2 ->
            if not (T.TVar.equal t1 t2) then
              Error.report (Error.or_pattern_type_vars_mismatch ~pos t_name);
            Some t1)
      in
      let _ = StrMap.merge check_type types1 types2 in
      (* Check values *)
      let vals1 = Name.Map.of_list (List.map (fun (n, _, sch) -> (n, sch)) info1.mi_vals) in
      let vals2 = Name.Map.of_list (List.map (fun (n, _, sch) -> (n, sch)) info2.mi_vals) in
      let check_val v_name sch1 sch2 =
        merge_opts sch1 sch2
          ~on_mismatch:(fun () -> Error.report (Error.or_pattern_vars_mismatch ~pos ~pp v_name))
          ~on_both:(fun sch1 sch2 ->
            Option.iter (fun check -> check sch1 sch2) check_schemes;
            Some sch1)
      in
      let _ = Name.Map.merge check_val vals1 vals2 in
      Some info1)

let intersect ?check_schemes ~pp ~pos ~scope penv1 penv2 =
  let ren =
    Name.Map.fold (fun name info2 ren ->
      match Name.Map.find_opt name penv1.val_map with
      | Some info1 -> T.Ren.add_var ren info2.vi_var info1.vi_var
      | None -> ren
    ) penv2.val_map (T.Ren.empty ~scope)
  in
  let penv =
    { tvar_map   =
        StrMap.merge (intersect_tvar_info ~pos) penv1.tvar_map penv2.tvar_map;
      tvar_tab   =
        T.TVar.Map.merge (intersect_tvar_pos ~pos) penv1.tvar_tab penv2.tvar_tab;
      val_map    =
        Name.Map.merge (intersect_val_info ~check_schemes ~pp ~pos) penv1.val_map penv2.val_map;
      module_map =
        StrMap.merge (intersect_module_info ~check_schemes ~pp ~pos) penv1.module_map penv2.module_map
    }
  in
  (penv, ren)

(* ========================================================================= *)

let introduce_type_param (env, ren) (pos, _, x) =
  let (env, y) =
    Env.add_anon_tvar ~pos ~pp_uid:(T.TVar.pp_uid x) env (T.TVar.kind x) in
  (env, T.Ren.add_tvar ren x y)

let introduce_anon_tvar x (_, pos) (env, ren) =
  let (env, y) =
    Env.add_anon_tvar ~pos ~pp_uid:(T.TVar.pp_uid x) env (T.TVar.kind x) in
  (env, T.Ren.add_tvar ren x y)

let introdce_tvar_alias name info (env, ren) =
  let x = T.Ren.rename_tvar ren info.ti_tvar in
  let env = Env.add_tvar_alias
    ~public:info.ti_public ~pos:info.ti_pos env name x in
  (env, ren)

let introduce_val name info (env, ren) =
  let sch = T.Ren.rename_scheme ren info.vi_scheme in
  let name = NameUtils.rename ren name in
  let (env, x) = Env.add_val ~public:info.vi_public env name sch in
  (env, T.Ren.add_var ren info.vi_var x)

let introduce_module_types ~pos types (env, ren) =
  let env =
    List.fold_left
      (fun env (name, x) ->
        let x = T.Ren.rename_tvar ren x in
        Env.add_tvar_alias ~pos ~public:true env name x)
      env types
  in (env, ren)

let introduce_module_vals vals (env, ren) =
  List.fold_left
    (fun (env, ren) (name, x, sch) ->
      let sch = T.Ren.rename_scheme ren sch in
      let name = NameUtils.rename ren name in
      let (env, y) = Env.add_val ~public:true env name sch in
      (env, T.Ren.add_var ren x y))
    (env, ren) vals

let introduce_module name info (env, ren) =
  let pos = info.mi_pos in
  let env = Env.enter_module env in
  let (env, ren) =
    (env, ren)
    |> introduce_module_types ~pos info.mi_types
    |> introduce_module_vals info.mi_vals
  in
  let env = Env.leave_module ~public:info.mi_public env name in
  (env, ren)

let extend env tvars penv =
  let (env, scope) = Env.enter_scope env in
  let (env, ren) =
    List.fold_left introduce_type_param (env, T.Ren.empty ~scope) tvars in
  let (env, _) = Env.enter_scope env in
  let (env, ren) =
    (env, ren)
    |> T.TVar.Map.fold introduce_anon_tvar penv.tvar_tab
    |> StrMap.fold introdce_tvar_alias penv.tvar_map
    |> Name.Map.fold introduce_val penv.val_map
    |> StrMap.fold introduce_module penv.module_map
  in
  let tvars =
    List.map (fun (_, name, x) -> (name, T.Ren.rename_tvar ren x)) tvars in
  (env, scope, tvars, ren)
