(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)
(*
open Common

module StrMap = Map.Make(String)

type pp_info = {
  pp_base_name : string;
  pp_names     : S.tvar S.path list;
  pp_pos       : Position.t option
}

type t = {
  mod_stack : ModStack.t;
    (** The module stack *)

  adt_map : Module.adt_info T.TVar.Map.t;
    (** Definition of ADT associated with a type variable *)

  method_map : (T.var * T.scheme) StrMap.t T.TVar.Map.t;
    (** Methods associated with a type variable *)

  pp_map : pp_info T.TVar.Map.t;
    (** Additional metadata used for pretty-printing of types *)

  scope : T.scope
    (** Scope of type variables *)
}

let mk_builtin_pp_info (name, x) =
  let info =
    { pp_base_name = name;
      pp_names     = [ NPName name ];
      pp_pos       = None
    }
  in (x, info)

let empty =
  { mod_stack    = ModStack.toplevel;
    adt_map      = T.TVar.Map.singleton T.BuiltinType.tv_unit Module.unit_info;
    method_map   = T.TVar.Map.empty;
    pp_map       =
      T.BuiltinType.all
      |> List.map mk_builtin_pp_info
      |> List.to_seq |> T.TVar.Map.of_seq;
    scope        =
      T.BuiltinType.all
      |> List.map snd
      |> List.fold_left T.Scope.add T.Scope.initial
  }

let add_poly_var ?(public=false) env x sch =
  let mod_stack, y = ModStack.add_var ~public env.mod_stack x sch in
  { env with mod_stack }, y

let add_mono_var ?(public=false) env x tp =
  add_poly_var ~public env x (T.Scheme.of_type tp)

let add_poly_implicit ?(public=false) env name sch on_use =
  let mod_stack, x =
    ModStack.add_implicit ~public env.mod_stack name sch on_use in
  { env with mod_stack }, x

let add_mono_implicit ?(public=false) env name tp on_use =
  add_poly_implicit ~public env name (T.Scheme.of_type tp) on_use

let add_the_label env eff tp0 eff0 =
  add_mono_var env "#label" (T.Type.t_label eff tp0 eff0)

let add_method_fn ~public env x name =
  { env with mod_stack = ModStack.add_method_fn ~public env.mod_stack x name }

let add_tvar ?pos ?(public=false) env name kind =
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

let add_anon_tvar ?pos ?(name="T") env kind =
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

let add_data env x info =
  assert (not (T.TVar.Map.mem x env.adt_map));
  { env with
    adt_map = T.TVar.Map.add x info env.adt_map
  }

let add_ctor ?(public=false) env name idx info =
  { env with
    mod_stack = ModStack.add_ctor ~public env.mod_stack name idx info
  }

let lookup_method_map env owner =
  match T.TVar.Map.find_opt owner env.method_map with
  | Some map -> map
  | None     -> StrMap.empty

let add_poly_method ?(public=false) env owner name sch =
  (* TODO: implement method visibility *)
  let x = Var.fresh ~name () in
  let method_map =
    lookup_method_map env owner |> StrMap.add name (x, sch) in
  { env with
    method_map = T.TVar.Map.add owner method_map env.method_map
  }, x

let lookup_module env =
  ModStack.lookup_module env.mod_stack

let lookup_var env =
  ModStack.lookup_var env.mod_stack

let lookup_implicit env =
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

let lookup_the_label env =
  match lookup_var env (NPName "#label") with
  | None -> None
  | Some(VI_Var (x, sch)) ->
    let (eff, tp0, eff0) = scheme_to_label sch in
    Some(x, eff, tp0, eff0)
  | Some(VI_Ctor _ | VI_MethodFn _) ->
    assert false

let lookup_ctor env =
  ModStack.lookup_ctor env.mod_stack

let lookup_tvar env =
  ModStack.lookup_tvar env.mod_stack

let lookup_the_effect env =
  lookup_tvar env (NPName "#effect")

let lookup_adt env x =
  T.TVar.Map.find_opt x env.adt_map

let lookup_method env owner name =
  StrMap.find_opt name (lookup_method_map env owner)

let lookup_tvar_pp_info env x =
  T.TVar.Map.find_opt x env.pp_map

let incr_level env =
  { env with scope = T.Scope.incr_level env.scope }

let scope env = env.scope

let extend_scope env (sch : T.scheme) = 
  let sch = T.Scheme.refresh sch in
  let env = 
    { env with 
    scope = List.fold_left T.Scope.add_named env.scope sch.sch_targs 
    } in
  (env, sch)

let level env = T.Scope.level env.scope

let fresh_uvar env kind =
  T.Type.fresh_uvar ~scope:env.scope kind

let enter_module env =
  { env with mod_stack = ModStack.enter_module env.mod_stack }

let leave_module env ~public name =
  { env with mod_stack = ModStack.leave_module ~public env.mod_stack name }

let open_module env ~public m =
  { env with mod_stack = ModStack.open_module env.mod_stack ~public m }
*)
