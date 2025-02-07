(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving module paths *)

open Common

let rec lookup_module env (path : S.module_name S.path) =
  let result =
    match path.data with
    | NPName name -> Env.lookup_module env name
    | NPSel(prefix, name) ->
      let prefix_modl = lookup_module env prefix in
      Module.lookup_module prefix_modl name
  in
  match result with
  | Some m -> m
  | None -> Error.fatal (Error.unbound_module path)

(* ========================================================================= *)

let tr_type_info ~pos env (info : Module.type_info) =
  match info with
  | TI_Type tp -> tp
  | TI_Parameter uid ->
    begin match Env.check_type_param ~pos env uid with
    | StValid x -> T.Type.t_var x
    | StNotGeneralized(name, decl_pos) ->
      Error.fatal
        (Error.ungeneralizable_type_param ~pos ~decl_pos name)
    | StRejected(name, decl_pos) ->
      Error.fatal
        (Error.rejected_type_param_used ~pos ~decl_pos name)
    end

let lookup_type env (path : S.tvar S.path) =
  let result =
    match path.data with
    | NPName name -> Env.lookup_tvar env name
    | NPSel(prefix, name) ->
      let prefix_modl = lookup_module env prefix in
      Module.lookup_tvar prefix_modl name
  in
  match result with
  | Some info -> tr_type_info ~pos:path.pos env info
  | None -> Error.fatal (Error.unbound_type path)

(* ========================================================================= *)

let tr_val_info ~pos ~env (info : Module.val_info) =
  match info with
  | VI_Var(x, sch) ->
    ({ T.pos; T.data = T.EVar x }, sch)

  | VI_Ctor(idx, info) ->
    let ctor = List.nth info.adt_ctors idx in
    let sch = {
        T.sch_targs = info.adt_args @ ctor.ctor_targs;
        T.sch_named = ctor.ctor_named;
        T.sch_body  = T.Type.t_pure_arrows ctor.ctor_arg_schemes info.adt_type
      } in
    (ExprUtils.ctor_func ~pos idx info, sch)

  | VI_Parameter uid ->
    begin match Env.check_val_param ~pos env uid with
    | StValid(x, sch) ->
      ({ T.pos; T.data = T.EVar x }, sch)
    | StNotGeneralized(name, decl_pos) ->
      Error.fatal
        (Error.ungeneralizable_named_param
          ~pos ~decl_pos ~pp:(Env.pp_tree env)
          name)
    | StRejected(name, decl_pos) ->
      Error.fatal
        (Error.rejected_named_param_used
          ~pos ~decl_pos ~pp:(Env.pp_tree env)
          name)
    end

(** Same as [tr_val_info], but assumes that the value is always a variable,
  e.g. for implicits or methods *)
let tr_var_info ~pos ~env (info : Module.val_info) =
  match info with
  | VI_Var(x, sch) -> (x, sch)
  
  | VI_Ctor _ ->
    (* implicits and methods cannot be constructors *)
    assert false

  | VI_Parameter uid ->
    begin match Env.check_val_param ~pos env uid with
    | StValid(x, sch) -> (x, sch)
    | StNotGeneralized(name, decl_pos) ->
      Error.fatal
        (Error.ungeneralizable_named_param
          ~pos ~decl_pos ~pp:(Env.pp_tree env)
          name)
    | StRejected(name, decl_pos) ->
      Error.fatal
        (Error.rejected_named_param_used
          ~pos ~decl_pos ~pp:(Env.pp_tree env)
          name)
    end

let lookup_var env (path : S.var S.path) =
  let result =
    match path.data with
    | NPName name -> Env.lookup_var env name
    | NPSel(prefix, name) ->
      let prefix_modl = lookup_module env prefix in
      Module.lookup_val prefix_modl (NVar name)
  in
  match result with
  | Some info -> tr_val_info ~pos:path.pos ~env info
  | None -> Error.fatal (Error.unbound_var path)

let lookup_implicit env (path : S.iname S.path) =
  let result =
    match path.data with
    | NPName name -> Env.lookup_implicit env name
    | NPSel(prefix, name) ->
      let prefix_modl = lookup_module env prefix in
      Module.lookup_val prefix_modl (NImplicit name)
  in
  match result with
  | Some info -> tr_val_info ~pos:path.pos ~env info
  | None -> Error.fatal (Error.unbound_implicit path)

let lookup_ctor env (path : S.ctor_name S.path) =
  let result =
    match path.data with
    | NPName name -> Env.lookup_ctor env name
    | NPSel(prefix, name) ->
      let prefix_modl = lookup_module env prefix in
      Module.lookup_ctor prefix_modl name
  in
  match result with
  | Some info -> info
  | None -> Error.fatal (Error.unbound_ctor path)

let lookup_adt env (cpath : S.ctor_name S.path) x =
  match cpath.data with
  | NPName _ -> Env.lookup_adt env x
  | NPSel(prefix, _) ->
    let prefix_modl = lookup_module env prefix in
    Module.lookup_adt prefix_modl x

(* ========================================================================= *)
let try_lookup_tvar ~pos env name =
  Option.map (tr_type_info ~pos env) (Env.lookup_tvar env name)

let try_lookup_tvar_in_module ~pos env modl name =
  Option.map (tr_type_info ~pos env) (Module.lookup_tvar modl name)

let try_lookup_val ~pos env name =
  Option.map (tr_val_info ~pos ~env) (Env.lookup_val env name)

let try_lookup_val_in_module ~pos env modl name =
  Option.map (tr_val_info ~pos ~env) (Module.lookup_val modl name)

let try_lookup_implicit ~pos env name =
  Option.map (tr_var_info ~pos ~env) (Env.lookup_implicit env name)

let try_lookup_the_label ~pos env =
  try_lookup_implicit ~pos env the_label_name

let try_lookup_method ~pos env own name =
  Option.map (tr_var_info ~pos ~env) (Env.lookup_method env own name)
