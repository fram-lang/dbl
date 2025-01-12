(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

open Common

type on_use = Position.t -> unit

type t = {
  cur_module : Module.t;
    (** Currently defined module *)

  mod_stack : Module.t list;
    (** Stack of modules which are currently being defined *)

  pp_tree : PPTree.t;
    (** Pretty-printing information for types *)

  scope : T.scope
    (** Scope of type variables *)
}
(*
let mk_builtin_pp_info (name, x) =
  let info =
    { pp_base_name = name;
      pp_names     = [ NPName name ];
      pp_pos       = None
    }
  in (x, info)
*)
let empty =
  { cur_module   = Module.toplevel;
    mod_stack    = [];
    pp_tree      =
      T.BuiltinType.all
      |> List.fold_left
          (fun pp_tree (name, x) ->
            PPTree.add pp_tree name (T.TVar.uid x))
          PPTree.empty;
    scope        =
      T.BuiltinType.all
      |> List.map snd
      |> List.fold_left T.Scope.add T.Scope.initial
  }

let add_poly_var ?(public=false) ?(on_use=ignore) env x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let mod_stack, y = ModStack.add_var ~public env.mod_stack x sch in
  { env with mod_stack }, y

let add_mono_var ?(public=false) env x tp =
  add_poly_var ~public env x (T.Scheme.of_type tp)
*)
let add_poly_implicit ?(public=false) ?(on_use=ignore) env name sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let mod_stack, x =
    ModStack.add_implicit ~public env.mod_stack name sch on_use in
  { env with mod_stack }, x

let add_mono_implicit ?(public=false) env name tp on_use =
  add_poly_implicit ~public env name (T.Scheme.of_type tp) on_use
*)
let add_the_label env tp =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
  (* add_mono_var env "#label" (T.Type.t_label eff tp0 eff0) *)

let add_method_fn ~public env x name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { env with mod_stack = ModStack.add_method_fn ~public env.mod_stack x name }
*)
let add_existing_tvar ?pos ?(public=false) ?(on_use=ignore) env name x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_existing_anon_tvar ?pos ?(name="T") ?(on_use=ignore) env x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_tvar ?pos ?(public=false) ?(on_use=ignore) env name kind =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let mod_stack, x = ModStack.add_tvar ~public env.mod_stack name kind in
  let pp_info = 
    { pp_base_name = name;
      pp_names     = [ NPName name ];
      pp_pos       = pos
    } in
  { env with
    mod_stack;
    pp_map   = T.TVar.Map.add x pp_info env.pp_map;
    scope    = T.Scope.add env.scope x
  }, x

let add_the_effect ?pos env =
  let mod_stack, x =
    ModStack.add_tvar env.mod_stack ~public:false "#effect" T.Kind.k_effect in
  let pp_info =
    { pp_base_name = "effect";
      pp_names     = [];
      pp_pos       = pos
    } in
  { env with
    mod_stack;
    pp_map   = T.TVar.Map.add x pp_info env.pp_map;
    scope    = T.Scope.add env.scope x
  }, x
*)
let add_anon_tvar ?pos ?(name="T") ?(on_use=ignore) env kind =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_tvar_alias ?pos ?(public=false) env name x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let x = T.TVar.fresh kind in
  let pp_info =
    { pp_base_name = name;
      pp_names     = [];
      pp_pos       = pos
    } in
  { env with
    pp_map = T.TVar.Map.add x pp_info env.pp_map;
    scope  = T.Scope.add env.scope x
  }, x
*)
(*
let add_the_effect_alias env tp =
  assert (T.Kind.view (T.Type.kind tp) = KEffect);
  { env with
    mod_stack = ModStack.add_type_alias env.mod_stack ~public:false "#effect" tp
  }

let add_type_alias ?(public=false) env name tp =
  let pp_map =
    match T.Type.view tp with
    | TVar x ->
      let pp_info =
        match T.TVar.Map.find_opt x env.pp_map with
        | Some pp_info ->
          { pp_info with pp_names = NPName name :: pp_info.pp_names }
        | None -> assert false
      in
      T.TVar.Map.add x pp_info env.pp_map
    | _ -> env.pp_map
  in
  { env with
    mod_stack = ModStack.add_type_alias ~public env.mod_stack name tp;
    pp_map    = pp_map
  }
*)
let add_data ?(public=false) env x info =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  assert (not (T.TVar.Map.mem x env.adt_map));
  { env with
    adt_map = T.TVar.Map.add x info env.adt_map
  }
*)
let add_ctor ?(public=false) env name idx info =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { env with
    mod_stack = ModStack.add_ctor ~public env.mod_stack name idx info
  }

let lookup_method_map env owner =
  match T.TVar.Map.find_opt owner env.method_map with
  | Some map -> map
  | None     -> StrMap.empty
*)
let add_poly_method ?(public=false) ?(on_use=ignore) env owner name sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  (* TODO: implement method visibility *)
  let x = Var.fresh ~name () in
  let method_map =
    lookup_method_map env owner |> StrMap.add name (x, sch) in
  { env with
    method_map = T.TVar.Map.add owner method_map env.method_map
  }, x

let lookup_module env =
  ModStack.lookup_module env.mod_stack
*)
let lookup_var env =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  ModStack.lookup_var env.mod_stack
*)
let lookup_implicit env =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  ModStack.lookup_implicit env.mod_stack

let scheme_to_label sch =
  match sch with
  | { T.sch_targs = []; sch_named = []; sch_body } ->
    begin match T.Type.view sch_body with
    | TLabel(eff, tp0, eff0) -> (eff, tp0, eff0)
    | _ -> assert false
    end
  | _ -> assert false

let add_the_label_sch env sch =
  let (eff, tp0, eff0) = scheme_to_label sch in
  add_the_label env eff tp0 eff0
*)
let lookup_the_label env =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  match lookup_var env (NPName "#label") with
  | None -> None
  | Some(VI_Var (x, sch)) ->
    let (eff, tp0, eff0) = scheme_to_label sch in
    Some(x, eff, tp0, eff0)
  | Some(VI_Ctor _ | VI_MethodFn _) ->
    assert false

let lookup_ctor env =
  ModStack.lookup_ctor env.mod_stack
*)
let lookup_tvar env x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  ModStack.lookup_tvar env.mod_stack

let lookup_the_effect env =
  lookup_tvar env (NPName "#effect")
*)
let lookup_adt env x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  T.TVar.Map.find_opt x env.adt_map
*)
let lookup_method env owner name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  StrMap.find_opt name (lookup_method_map env owner)

let lookup_tvar_pp_info env x =
  T.TVar.Map.find_opt x env.pp_map
*)
let incr_level env =
  { env with scope = T.Scope.incr_level env.scope }

let scope env = env.scope
(*
let extend_scope env (sch : T.scheme) = 
  let sch = T.Scheme.refresh sch in
  let env = 
    { env with 
    scope = List.fold_left T.Scope.add_named env.scope sch.sch_targs 
    } in
  (env, sch)
*)
let level env = T.Scope.level env.scope

let pp_tree env = env.pp_tree

let fresh_uvar env kind =
  T.Type.fresh_uvar ~scope:env.scope kind

let enter_module env =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { env with mod_stack = ModStack.enter_module env.mod_stack }
*)
let leave_module env ~public name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { env with mod_stack = ModStack.leave_module ~public env.mod_stack name }
*)
let open_module env ~public m =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { env with mod_stack = ModStack.open_module env.mod_stack ~public m }
*)
