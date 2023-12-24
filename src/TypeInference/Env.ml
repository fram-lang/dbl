(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

module StrMap = Map.Make(String)

(** Information about ADT definition *)
type adt_info = {
  adt_proof : T.expr;
    (** A computationally irrelevant expression that give a proof that given
      type is an ADT *)

  adt_ctors : T.ctor_decl list
    (** List of constructors of an ADT *)
}

type ctor_info = {
  ci_name      : string;
  ci_index     : int;
  ci_proof     : T.expr;
  ci_arg_types : T.typ list;
  ci_type      : T.typ
}

type t = {
  var_map : (T.var * T.scheme) StrMap.t;
    (** Information about regular variable names *)

  tvar_map : T.tvar StrMap.t;
    (** Information about named type variables *)

  implicit_map : (T.var * T.scheme * (Position.t -> unit)) StrMap.t;
    (** Information about named implicits *)

  ctor_map : ctor_info StrMap.t;
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

let add_data env x proof ctors =
  assert (not (T.TVar.Map.mem x env.adt_map));
  let info =
    { adt_proof = proof;
      adt_ctors = ctors
    }
  in
  { env with
    adt_map = T.TVar.Map.add x info env.adt_map
  }

let add_ctor env name info =
  { env with
    ctor_map = StrMap.add name info env.ctor_map
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
