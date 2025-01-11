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

let option_proof tp =
  let tp_expr = make_nowhere (T.TE_Type tp) in
  make_nowhere (T.EInst(make_nowhere T.EOptionPrf, [tp_expr], []))

let mk_none ~pos tp =
  { T.pos  = pos;
    T.data = T.ECtor(option_proof tp, 0, [], [], [])
  }

let mk_some_poly ~pos tp e =
  { T.pos  = pos;
    T.data = T.ECtor(option_proof tp, 1, [], [], [e])
  }
