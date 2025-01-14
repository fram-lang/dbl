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
  
  mi_vars      : (S.var * T.var * T.scheme) list;
    (** The public variables of the module. *)

  mi_implicits : (S.iname * T.var * T.scheme) list;
    (** The public implicit variables of the module. *)

  methods      : (T.tvar * S.method_name * T.var * T.scheme) list;
    (** The public methods of the module. *)
}

type is_anon = bool

type t = {
  tvar_map : tvar_info StrMap.t;
    (** Map from type variable names to type variable information. *)

  tvar_tab : (is_anon * Position.t) T.TVar.Map.t;
    (** Unif type variables bound by the pattern. It stores the place of
      binding. *)

  var_map  : var_info StrMap.t;
    (** Map from variable names to variable information. *)

  implicit_map : var_info StrMap.t;
    (** Map from implicit variable names to variable information. *)

  method_map : var_info StrMap.t T.TVar.Map.t;
    (** Map from type variable to method table. *)

  module_map : module_info StrMap.t;
    (** Map from module names to module information. *)
}

let empty =
  { tvar_map     = StrMap.empty;
    tvar_tab     = T.TVar.Map.empty;
    var_map      = StrMap.empty;
    implicit_map = StrMap.empty;
    method_map   = T.TVar.Map.empty;
    module_map   = StrMap.empty
  }

let singleton_tvar ~public ~pos name x =
  let info =
    { ti_public = public;
      ti_pos    = pos;
      ti_tvar   = x
    } in
  { empty with
    tvar_map = StrMap.singleton name info;
    tvar_tab = T.TVar.Map.singleton x (false, pos)
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

let singleton_var ~public ~pos name sch =
  let x = Var.fresh ~name () in
  let info =
    { vi_public = public;
      vi_pos    = pos;
      vi_var    = x;
      vi_scheme = sch
    } in
  ({ empty with var_map = StrMap.singleton name info }, x)

let singleton_implicit ~public ~pos name sch =
  let x = Var.fresh ~name () in
  let info =
    { vi_public = public;
      vi_pos    = pos;
      vi_var    = x;
      vi_scheme = sch
    } in
  ({ empty with implicit_map = StrMap.singleton name info }, x)

let singleton_method ~public ~pos owner name sch =
  let x = Var.fresh ~name () in
  let info =
    { vi_public = public;
      vi_pos    = pos;
      vi_var    = x;
      vi_scheme = sch
    } in
  let penv =
    { empty with
      method_map = T.TVar.Map.singleton owner (StrMap.singleton name info)
    } in
  (penv, x)

let singleton_module ~public ~pos ~types ~vars ~implicits ~methods modname =
  let info =
    { mi_public    = public;
      mi_pos       = pos;
      mi_types     = types;
      mi_vars      = vars;
      mi_implicits = implicits;
      methods      = methods
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

let add_var ~public ~pos penv name x sch =
  match StrMap.find_opt name penv.var_map with
  | None ->
    let info =
      { vi_public = public;
        vi_pos    = pos;
        vi_var    = x;
        vi_scheme = sch
      } in
    { penv with var_map = StrMap.add name info penv.var_map }

  | Some info ->
    Error.report
      (Error.duplicate_var_in_pattern ~pos ~ppos:info.vi_pos name);
    penv

let add_implicit ~public ~pos penv name x sch =
  match StrMap.find_opt name penv.implicit_map with
  | None ->
    let info =
      { vi_public = public;
        vi_pos    = pos;
        vi_var    = x;
        vi_scheme = sch
      } in
    { penv with implicit_map = StrMap.add name info penv.implicit_map }

  | Some info ->
    Error.report
      (Error.duplicate_implicit_in_pattern ~pos ~ppos:info.vi_pos name);
    penv

let add_method ~public ~pos ~env penv owner name x sch =
  let tab =
    match T.TVar.Map.find_opt owner penv.method_map with
    | Some tab -> tab
    | None     -> StrMap.empty
  in
  match StrMap.find_opt name tab with
  | None ->
    let info =
      { vi_public = public;
        vi_pos    = pos;
        vi_var    = x;
        vi_scheme = sch
      } in
    { penv with
      method_map =
        T.TVar.Map.add owner (StrMap.add name info tab) penv.method_map
    }

  | Some info ->
    Error.report
      (Error.duplicate_method_in_pattern
        ~pos ~ppos:info.vi_pos ~env owner name);
    penv

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

let join_var_info name info1 info2 =
  match info1, info2 with
  | None,       None       -> None
  | Some info,  None       -> Some info
  | None,       Some info  -> Some info
  | Some info1, Some info2 ->
    Error.report
      (Error.duplicate_var_in_pattern
        ~pos:info1.vi_pos ~ppos:info2.vi_pos name);
    Some info2

let join_implicit_info name info1 info2 =
  match info1, info2 with
  | None,       None       -> None
  | Some info,  None       -> Some info
  | None,       Some info  -> Some info
  | Some info1, Some info2 ->
    Error.report
      (Error.duplicate_implicit_in_pattern
        ~pos:info1.vi_pos ~ppos:info2.vi_pos name);
    Some info2

let join_method_info ~env owner name info1 info2 =
  match info1, info2 with
  | None,       None       -> None
  | Some info,  None       -> Some info
  | None,       Some info  -> Some info
  | Some info1, Some info2 ->
    Error.report
      (Error.duplicate_method_in_pattern
        ~pos:info1.vi_pos ~ppos:info2.vi_pos ~env owner name);
    Some info2

let join_method_tabs ~env owner tab1 tab2 =
  match tab1, tab2 with
  | None,      None       -> None
  | Some tab,  None       -> Some tab
  | None,      Some tab   -> Some tab
  | Some tab1, Some tab2 ->
    Some (StrMap.merge (join_method_info ~env owner) tab1 tab2)

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

let join ~env penv1 penv2 =
  { tvar_map     =
      StrMap.merge join_tvar_info penv1.tvar_map penv2.tvar_map;
    tvar_tab     =
      T.TVar.Map.merge join_tvar_pos penv1.tvar_tab penv2.tvar_tab;
    var_map      =
      StrMap.merge join_var_info penv1.var_map penv2.var_map;
    implicit_map =
      StrMap.merge join_implicit_info penv1.implicit_map penv2.implicit_map;
    method_map   =
      T.TVar.Map.merge (join_method_tabs ~env)
        penv1.method_map penv2.method_map;
    module_map   =
      StrMap.merge join_module_info penv1.module_map penv2.module_map
  }

(* ========================================================================= *)

let introduce_anon_tvar x (_, pos) env =
  Env.add_existing_anon_tvar ~pos env x

let introdce_tvar_alias name info env =
  Env.add_tvar_alias ~public:info.ti_public ~pos:info.ti_pos
    env name info.ti_tvar

let introduce_var name info env =
  Env.add_existing_var ~public:info.vi_public
    env name info.vi_var info.vi_scheme

let introduce_implicit name info env =
  Env.add_existing_implicit ~public:info.vi_public
    env name info.vi_var info.vi_scheme

let introduce_method owner name info env =
  Env.add_existing_method ~public:info.vi_public
    env owner name info.vi_var info.vi_scheme

let introduce_all_methods owner tab env =
  StrMap.fold (introduce_method owner) tab env

let introduce_module_types ~pos types env =
  List.fold_left
    (fun env (name, x) -> Env.add_existing_tvar ~pos env name x)
    env types

let introduce_module_vars vars env =
  List.fold_left
    (fun env (name, x, sch) -> Env.add_existing_var env name x sch)
    env vars

let introduce_module_implicits implicits env =
  List.fold_left
    (fun env (name, x, sch) -> Env.add_existing_implicit env name x sch)
    env implicits

let introduce_module_methods methods env =
  List.fold_left
    (fun env (owner, name, x, sch) ->
      Env.add_existing_method env owner name x sch)
    env methods

let introduce_module name info env =
  let pos = info.mi_pos in
  env
  |> Env.enter_module
  |> introduce_module_types ~pos info.mi_types
  |> introduce_module_vars info.mi_vars
  |> introduce_module_implicits info.mi_implicits
  |> introduce_module_methods info.methods
  |> (fun env -> Env.leave_module ~public:info.mi_public env name)

let extend env penv =
  env
  |> T.TVar.Map.fold introduce_anon_tvar penv.tvar_tab
  |> StrMap.fold introdce_tvar_alias penv.tvar_map
  |> StrMap.fold introduce_var penv.var_map
  |> StrMap.fold introduce_implicit penv.implicit_map
  |> T.TVar.Map.fold introduce_all_methods penv.method_map
  |> StrMap.fold introduce_module penv.module_map

(* ========================================================================= *)

let type_names penv =
  StrMap.bindings penv.tvar_map |> List.map fst

let var_names penv =
  StrMap.bindings penv.var_map |> List.map fst

let implicit_names penv =
  StrMap.bindings penv.implicit_map |> List.map fst
