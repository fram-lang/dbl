(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving module paths *)

open Common

let rec lookup_module env (path : S.module_name S.path) =
  match path.data with
  | NPName name ->
    begin match Env.lookup_module env name with
    | Some m -> m
    | None   -> Error.fatal (Error.unbound_module path)
    end

  | NPSel(prefix, name) ->
    let prefix_modl = lookup_module env prefix in
    begin match Module.lookup_module prefix_modl name with
    | Some m -> m
    | None   -> Error.fatal (Error.unbound_module path)
    end

let lookup_type env (path : S.tvar S.path) =
  match path.data with
  | NPName name ->
    begin match Env.lookup_tvar env name with
    | Some(tp, on_use) ->
      on_use path.pos;
      tp
    | None -> Error.fatal (Error.unbound_type path)
    end

  | NPSel(prefix, name) ->
    let prefix_modl = lookup_module env prefix in
    begin match Module.lookup_tvar prefix_modl name with
    | Some(tp, on_use) ->
      on_use path.pos;
      tp
    | None -> Error.fatal (Error.unbound_type path)
    end

let lookup_var env (path : S.var S.path) =
  match path.data with
  | NPName name ->
    begin match Env.lookup_var env name with
    | Some(info, on_use) ->
      on_use path.pos;
      info
    | None -> Error.fatal (Error.unbound_var path)
    end

  | NPSel(prefix, name) ->
    let prefix_modl = lookup_module env prefix in
    begin match Module.lookup_var prefix_modl name with
    | Some(info, on_use) ->
      on_use path.pos;
      info
    | None -> Error.fatal (Error.unbound_var path)
    end

let lookup_implicit env (path : S.iname S.path) =
  match path.data with
  | NPName name ->
    begin match Env.lookup_implicit env name with
    | Some(x, sch, on_use) ->
      on_use path.pos;
      (x, sch)
    | None -> Error.fatal (Error.unbound_implicit path)
    end

  | NPSel(prefix, name) ->
    let prefix_modl = lookup_module env prefix in
    begin match Module.lookup_implicit prefix_modl name with
    | Some(x, sch, on_use) ->
      on_use path.pos;
      (x, sch)
    | None -> Error.fatal (Error.unbound_implicit path)
    end

let lookup_ctor env (path : S.ctor_name S.path) =
  match path.data with
  | NPName name ->
    begin match Env.lookup_ctor env name with
    | Some(idx, adt) -> (idx, adt)
    | None -> Error.fatal (Error.unbound_ctor path)
    end

  | NPSel(prefix, name) ->
    let prefix_modl = lookup_module env prefix in
    begin match Module.lookup_ctor prefix_modl name with
    | Some(idx, adt) -> (idx, adt)
    | None -> Error.fatal (Error.unbound_ctor path)
    end

let lookup_adt env (cpath : S.ctor_name S.path) x =
  match cpath.data with
  | NPName _ -> Env.lookup_adt env x
  | NPSel(prefix, _) ->
    let prefix_modl = lookup_module env prefix in
    Module.lookup_adt prefix_modl x
