(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraints generated during type inference *)

open Common

type t =
  | ResolveMethod : {
      hole       : T.poly_expr option BRef.t;
      vset       : Var.Set.t;
      pos        : Position.t;
      env        : 'st Env.t;
      method_env : 'st Env.t;
      self_tp    : T.typ;
      mname      : S.method_name;
      sch        : T.scheme;
    } -> t

let fix_scope ~pos new_tvars constr =
  match constr with
  | ResolveMethod c ->
    let env =
      T.TVar.Set.fold
        (fun x env -> Env.add_existing_anon_tvar ~pos env x)
        new_tvars c.env
    in
    ResolveMethod { c with env }

let fix_scopes ~pos new_tvars cs =
  List.map (fix_scope ~pos new_tvars) cs
