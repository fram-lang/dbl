(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of the translation *)

module S = Lang.ConE
module T = Lang.Core

(** Translate kind *)
let rec tr_kind k =
  match Lang.Unif.Kind.view k with
  | KType   -> T.Kind.Ex KType
  | KEffect -> T.Kind.Ex KEffect
  | KUVar u ->
    Lang.Unif.KUVar.set u Lang.Unif.Kind.k_type;
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
  T.VExtern("__Unit_Proof__", T.TData(T.Type.t_unit, T.Positive, [ ctor ]))

(** Proof that bool is an ADT *)
let v_bool_prf =
  let ctor_false = 
    { T.ctor_name      = "False";
      T.ctor_tvars     = [];
      T.ctor_arg_types = []
    } in
  let ctor_true =
    { T.ctor_name      = "True";
      T.ctor_tvars     = [];
      T.ctor_arg_types = []
    } in
  let data_tp = T.TData(
    T.Type.t_bool,
    T.Positive,
    [ ctor_false; ctor_true ]) in
  T.VExtern("__Bool_Proof__", data_tp)

(** Proof that option is an ADT *)
let v_option_prf =
  let arg = T.TVar.fresh T.KType in
  let ctor_none =
    { T.ctor_name      = "None";
      T.ctor_tvars     = [];
      T.ctor_arg_types = []
    } in
  let ctor_some =
    { T.ctor_name      = "Some";
      T.ctor_tvars     = [];
      T.ctor_arg_types = [ T.TVar arg ]
    } in
  let data_tp = T.TData(
    T.Type.t_option (T.TVar arg),
    T.Positive,
    [ ctor_none; ctor_some ]) in
  T.VExtern("__Option_Proof__", T.TForall(arg, data_tp))
