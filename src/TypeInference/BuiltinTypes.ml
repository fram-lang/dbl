(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on built-in types. *)

open Common

let mk_option tp =
  T.Type.t_app (T.Type.t_var T.BuiltinType.tv_option) tp

let mk_option_type_expr tp =
  { tp with T.data = T.TE_Option tp }

let mk_option_scheme tp =
  T.Scheme.of_type (mk_option tp)

let mk_option_scheme_expr tp =
  T.SchemeExpr.of_type_expr (mk_option_type_expr tp)

let scheme_to_option_arg (sch : T.scheme) =
  assert (T.Scheme.is_monomorphic sch);
  match T.Type.whnf sch.sch_body with
  | Whnf_Neutral(_, [arg]) -> arg
  | _ -> assert false

let mk_none ~pos ~pp tp =
  let make data = { T.pos = pos; T.pp = pp; T.data = data } in
  make (T.EInst(make (T.ECtor([], PE_Option tp, 0)), [], []))

let mk_some_poly ~pos ~pp tp e =
  let make data = { T.pos = pos; T.pp = pp; T.data = data } in
  make (T.EAppPoly(
    make (T.EInst(make (T.ECtor([], PE_Option tp, 1)), [], [])),
    e))
