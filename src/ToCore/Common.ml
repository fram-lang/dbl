(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of the translation *)
(*
module S = Lang.Unif
module T = Lang.Core

(** Translate kind *)
let rec tr_kind k =
  match S.Kind.view k with
  | KType   -> T.Kind.Ex KType
  | KEffect -> T.Kind.Ex KEffect
  | KEffrow -> T.Kind.Ex KEffect
  | KUVar u ->
    S.KUVar.set_safe u S.Kind.k_type;
    T.Kind.Ex KType
  | KArrow(k1, k2) ->
    let (Ex k1) = tr_kind k1 in
    let (Ex k2) = tr_kind k2 in
    T.Kind.Ex (KArrow(k1, k2))

(** Proof that unit is an ADT *)
let v_unit_prf =
  let ctor =
    { T.ctor_name      = "()";
      T.ctor_tvars     = [];
      T.ctor_arg_types = []
    } in
  T.VExtern("__Unit_Proof__", T.TData(T.Type.t_unit, T.TEffPure, [ ctor ]))

let v_unit =
  T.VCtor(T.EValue v_unit_prf, 0, [], [])
*)
