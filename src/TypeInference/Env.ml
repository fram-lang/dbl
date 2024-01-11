(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

module StrMap = Map.Make(String)

type adt_info = {
  adt_proof : T.expr;
  adt_args  : T.tvar list;
  adt_ctors : T.ctor_decl list;
  adt_type  : T.typ
}

type t = {
  var_map : (T.var * T.scheme) StrMap.t;
    (** Information about regular variable names *)

  tvar_map : T.tvar StrMap.t;
    (** Information about named type variables *)

  implicit_map : (T.var * T.scheme * (Position.t -> unit)) StrMap.t;
    (** Information about named implicits *)

  ctor_map : (int * adt_info) StrMap.t;
    (** Information about ADT constructors *)

  adt_map : adt_info T.TVar.Map.t;
    (** Definition of ADT associated with a type variable *)

  scope   : T.scope
    (** Scope of type variables *)
}

let empty =
  { var_map      = StrMap.empty;
    tvar_map     = StrMap.empty;
    implicit_map = StrMap.empty;
    ctor_map     = StrMap.empty;
    adt_map      = T.TVar.Map.empty;
    scope        = T.Scope.initial
  }

let add_poly_var env x sch =
  let y = Var.fresh ~name:x () in
  { env with
    var_map = StrMap.add x (y, sch) env.var_map
  }, y

let add_mono_var env x tp =
  add_poly_var env x { sch_tvars = []; sch_implicit = []; sch_body = tp }

let add_poly_implicit env name sch on_use =
  let x = Var.fresh ~name () in
  { env with
    implicit_map = StrMap.add name (x, sch, on_use) env.implicit_map
  }, x

let add_mono_implicit env name tp on_use =
  add_poly_implicit env name
    { sch_tvars = []; sch_implicit = []; sch_body = tp }
    on_use

let add_tvar env name kind =
  let x = T.TVar.fresh kind in
  { env with
    tvar_map = StrMap.add name x env.tvar_map;
    scope    = T.Scope.add env.scope x
  }, x

let add_anon_tvar env kind =
  let x = T.TVar.fresh kind in
  { env with
    scope = T.Scope.add env.scope x
  }, x

let add_data env x info =
  assert (not (T.TVar.Map.mem x env.adt_map));
  { env with
    adt_map = T.TVar.Map.add x info env.adt_map
  }

let add_ctor env name idx info =
  { env with
    ctor_map = StrMap.add name (idx, info) env.ctor_map
  }

let lookup_var env x =
  StrMap.find_opt x env.var_map

let lookup_implicit env name =
  StrMap.find_opt name env.implicit_map

let lookup_ctor env c =
  StrMap.find_opt c env.ctor_map

let lookup_tvar env x =
  StrMap.find_opt x env.tvar_map

let uvars env =
  T.UVar.Set.empty
  |> StrMap.fold
      (fun _ (_, sch) -> T.Scheme.collect_uvars sch)
      env.var_map

let scope env = env.scope

let fresh_uvar env kind =
  T.Type.fresh_uvar ~scope:env.scope kind

let open_scheme env sch =
  let sch = T.Scheme.refresh sch in
  let env = 
    { env with
      scope = List.fold_left T.Scope.add env.scope sch.sch_tvars
    } in
  let (env, ims) =
    List.fold_left_map
      (fun env (name, sch) ->
        let (env, x) = add_poly_implicit env name sch ignore in
        (env, (name, x, sch)))
      env
      sch.sch_implicit in
  (env, sch.sch_tvars, ims, sch.sch_body)
