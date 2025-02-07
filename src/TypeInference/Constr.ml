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
