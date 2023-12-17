(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

module StrMap = Map.Make(String)

type t = {
  var_map : (T.var * T.scheme) StrMap.t;
    (** Information about regular variable names *)

  implicit_map : (T.var * T.scheme * (Position.t -> unit)) StrMap.t;
    (** Information about named implicits *)

  scope   : T.scope
    (** Scope of type variables *)
}

let empty =
  { var_map      = StrMap.empty;
    implicit_map = StrMap.empty;
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

let add_anon_tvar env kind =
  let x = T.TVar.fresh kind in
  { env with
    scope = T.Scope.add env.scope x
  }, x

let lookup_var env x =
  StrMap.find_opt x env.var_map

let lookup_implicit env name =
  StrMap.find_opt name env.implicit_map

let uvars env =
  T.UVar.Set.empty
  |> StrMap.fold
      (fun _ (_, sch) -> T.Scheme.collect_uvars sch)
      env.var_map

let scope env = env.scope

let fresh_uvar env kind =
  T.Type.fresh_uvar ~scope:env.scope kind
