(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

module StrMap = Map.Make(String)

type adt_info = {
  adt_proof : T.expr;
  adt_args  : T.named_tvar list;
  adt_ctors : T.ctor_decl list;
  adt_type  : T.typ
}

type pp_info = {
  pp_base_name : string;
  pp_names     : string list;
  pp_pos       : Position.t option
}

type t = {
  var_map : (T.var * T.scheme) StrMap.t;
    (** Information about regular variable names *)

  tvar_map : T.typ StrMap.t;
    (** Information about named type variables *)

  implicit_map : (T.var * T.scheme * (Position.t -> unit)) StrMap.t;
    (** Information about named implicits *)

  ctor_map : (int * adt_info) StrMap.t;
    (** Information about ADT constructors *)

  adt_map : adt_info T.TVar.Map.t;
    (** Definition of ADT associated with a type variable *)

  method_map : (T.var * T.scheme) StrMap.t T.TVar.Map.t;
    (** Methods associated with a type variable *)

  pp_map : pp_info T.TVar.Map.t;
    (** Additional metadata used for pretty-printing of types *)

  scope   : T.scope
    (** Scope of type variables *)
}

let unit_info =
  { adt_proof = { T.pos = Position.nowhere; T.data = T.EUnitPrf };
    adt_args  = [];
    adt_ctors =
      [ { ctor_name        = "()";
          ctor_targs       = [];
          ctor_named       = [];
          ctor_arg_schemes = []
        } ];
    adt_type  = T.Type.t_unit
  }

let mk_builtin_pp_info (name, x) =
  let info =
    { pp_base_name = name;
      pp_names     = [ name ];
      pp_pos       = None
    }
  in (x, info)

let empty =
  { var_map      = StrMap.empty;
    tvar_map     =
      T.BuiltinType.all
      |> List.map (fun (name, tv) -> (name, T.Type.t_var tv))
      |> List.to_seq |> StrMap.of_seq;
    implicit_map = StrMap.empty;
    ctor_map     = StrMap.singleton "()" (0, unit_info);
    adt_map      = T.TVar.Map.singleton T.BuiltinType.tv_unit unit_info;
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

let add_poly_var env x sch =
  let y = Var.fresh ~name:x () in
  { env with
    var_map = StrMap.add x (y, sch) env.var_map
  }, y

let add_mono_var env x tp =
  add_poly_var env x (T.Scheme.of_type tp)

let add_poly_implicit env name sch on_use =
  let x = Var.fresh ~name () in
  { env with
    implicit_map = StrMap.add name (x, sch, on_use) env.implicit_map
  }, x

let add_mono_implicit env name tp on_use =
  add_poly_implicit env name (T.Scheme.of_type tp) on_use

let add_the_label env eff tp0 eff0 =
  add_mono_var env "#label" (T.Type.t_label eff tp0 eff0)

let add_tvar ?pos env name kind =
  let x = T.TVar.fresh kind in
  let pp_info = 
    { pp_base_name = name;
      pp_names     = [name];
      pp_pos       = pos
    } in
  { env with
    tvar_map = StrMap.add name (T.Type.t_var x) env.tvar_map;
    pp_map   = T.TVar.Map.add x pp_info env.pp_map;
    scope    = T.Scope.add env.scope x
  }, x

let add_the_effect ?pos env =
  let x = T.TVar.fresh T.Kind.k_effect in
  let pp_info =
    { pp_base_name = "effect";
      pp_names     = [];
      pp_pos       = pos
    } in
  { env with
    tvar_map = StrMap.add "#effect" (T.Type.t_var x) env.tvar_map;
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
  { env with tvar_map = StrMap.add "#effect" tp env.tvar_map }

let add_type_alias env name tp =
  let pp_map =
    match T.Type.view tp with
    | TVar x ->
      let pp_info =
        match T.TVar.Map.find_opt x env.pp_map with
        | Some pp_info -> { pp_info with pp_names = name :: pp_info.pp_names }
        | None -> assert false
      in
      T.TVar.Map.add x pp_info env.pp_map
    | _ -> env.pp_map
  in
  { env with
    tvar_map = StrMap.add name tp env.tvar_map;
    pp_map   = pp_map
  }

let add_data env x info =
  assert (not (T.TVar.Map.mem x env.adt_map));
  { env with
    adt_map = T.TVar.Map.add x info env.adt_map
  }

let add_ctor env name idx info =
  { env with
    ctor_map = StrMap.add name (idx, info) env.ctor_map
  }

let lookup_method_map env owner =
  match T.TVar.Map.find_opt owner env.method_map with
  | Some map -> map
  | None     -> StrMap.empty

let add_poly_method env owner name sch =
  let x = Var.fresh ~name () in
  let method_map =
    lookup_method_map env owner |> StrMap.add name (x, sch) in
  { env with
    method_map = T.TVar.Map.add owner method_map env.method_map
  }, x

let lookup_var env x =
  StrMap.find_opt x env.var_map

let lookup_implicit env name =
  StrMap.find_opt name env.implicit_map

let scheme_to_label sch =
  match sch with
  | { T.sch_targs = []; sch_named = []; sch_body } ->
    begin match T.Type.view sch_body with
    | TLabel(eff, tp0, eff0) -> (eff, tp0, eff0)
    | _ -> assert false
    end
  | _ -> assert false

let lookup_the_label env =
  match lookup_var env "#label" with
  | None -> None
  | Some(x, sch) ->
    let (eff, tp0, eff0) = scheme_to_label sch in
    Some(x, eff, tp0, eff0)

let lookup_ctor env c =
  StrMap.find_opt c env.ctor_map

let lookup_tvar env x =
  StrMap.find_opt x env.tvar_map

let lookup_the_effect env =
  lookup_tvar env "#effect"

let lookup_adt env x =
  T.TVar.Map.find_opt x env.adt_map

let lookup_method env owner name =
  StrMap.find_opt name (lookup_method_map env owner)

let lookup_tvar_pp_info env x =
  T.TVar.Map.find_opt x env.pp_map

let collect_adt_uvars (info : adt_info) uvars =
  uvars
  |> List.fold_right T.CtorDecl.collect_uvars info.adt_ctors
  |> T.Type.collect_uvars info.adt_type

let collect_method_uvars =
  StrMap.fold (fun _ (_, sch) -> T.Scheme.collect_uvars sch)

let uvars env =
  T.UVar.Set.empty
  |> StrMap.fold
      (fun _ (_, sch) -> T.Scheme.collect_uvars sch)
      env.var_map
  |> StrMap.fold
      (fun _ -> T.Type.collect_uvars)
      env.tvar_map
  |> StrMap.fold
      (fun _ (_, sch, _) -> T.Scheme.collect_uvars sch)
      env.implicit_map
  |> T.TVar.Map.fold
      (fun _ -> collect_adt_uvars)
      env.adt_map
  |> T.TVar.Map.fold
      (fun _ -> collect_method_uvars)
      env.method_map

let scope env = env.scope

let fresh_uvar env kind =
  T.Type.fresh_uvar ~scope:env.scope kind

let open_scheme env sch =
  let sch = T.Scheme.refresh sch in
  let env = 
    { env with
      scope = List.fold_left T.Scope.add_named env.scope sch.sch_targs
    } in
  let (env, ims) =
    List.fold_left_map
      (fun env (name, sch) ->
        let (env, x) =
          match name with
          | T.NLabel ->
            let (eff, tp0, eff0) = scheme_to_label sch in
            add_the_label env eff tp0 eff0
          | T.NVar x -> add_poly_var env x sch
          | T.NImplicit n -> add_poly_implicit env n sch ignore
        in
        (env, (name, x, sch)))
      env
      sch.sch_named in
  (env, sch.sch_targs, ims, sch.sch_body)
