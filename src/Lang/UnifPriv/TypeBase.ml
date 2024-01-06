(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open KindBase

type name = string

type tvar = TVar.t

type scope = TVar.Set.t

type uvar = {
  uid   : UID.t;
  kind  : kind;
  state : uvar_state BRef.t;
  scope : scope BRef.t
}

and uvar_state =
  | UV_UVar
  | UV_Type of typ

and typ = type_view
and effect = typ
and type_view =
  | TUnit
  | TUVar      of uvar
  | TVar       of tvar
  | TEffect    of TVar.Set.t * effect_end
  | TPureArrow of typ * typ
  | TArrow     of typ * typ * effect
  | TApp       of typ * typ

and effect_end =
  | EEClosed
  | EEUVar of uvar
  | EEVar  of tvar
  | EEApp  of typ * typ

type scheme = {
  sch_tvars    : tvar list;
  sch_implicit : (name * typ) list;
  sch_body     : typ
}

type ctor_decl = {
  ctor_name      : string;
  ctor_arg_types : typ list
}

let t_unit = TUnit

let t_uvar u = TUVar u

let t_var x = TVar x

let t_effect xs ee = TEffect(xs, ee)

let t_closed_effect xs =  TEffect(xs, EEClosed)

let t_pure_arrow tp1 tp2 = TPureArrow(tp1, tp2)

let t_arrow tp1 tp2 eff = TArrow(tp1, tp2, eff)

let t_app tp1 tp2 = TApp(tp1, tp2)

let rec view tp =
  match tp with
  | TUVar u ->
    begin match BRef.get u.state with
    | UV_UVar -> tp
    | UV_Type tp ->
      (* Path compression *)
      let tp = view tp in
      BRef.set u.state (UV_Type tp);
      tp
    end
  | TEffect(xs, EEUVar u) ->
    begin match view (TUVar u) with
    | TUVar u -> TEffect(xs, EEUVar u)
    | TVar x  -> TEffect(xs, EEVar x)
    | TApp(tp1, tp2) -> TEffect(xs, EEApp(tp1, tp2))
    | TEffect(ys, ee) -> TEffect(TVar.Set.union xs ys, ee)

    | TUnit | TPureArrow _ | TArrow _ ->
      failwith "Internal kind error"
    end
  | TEffect(xs, ee) -> tp

  | TUnit | TVar _ | TPureArrow _ | TArrow _ | TApp _ -> tp

let effect_view eff =
  match view eff with
  | TUVar u -> (TVar.Set.empty, EEUVar u)
  | TVar  x ->
    begin match KindBase.view (TVar.kind x) with
    | KEffect   -> (TVar.Set.empty, EEVar x)
    | KClEffect -> (TVar.Set.singleton x, EEClosed)
    | KType | KUVar _ | KArrow _ ->
      failwith "Internal kind error"
    end
  | TApp(tp1, tp2) -> (TVar.Set.empty, EEApp(tp1, tp2))

  | TEffect(xs, ee) -> (xs, ee)

  | TUnit | TPureArrow _ | TArrow _ ->
    failwith "Internal kind error"

module UVar = struct
  module Ordered = struct
    type t = uvar
    let compare u1 u2 = UID.compare u1.uid u2.uid
  end
  include Ordered

  let fresh ~scope kind = {
      uid   = UID.fresh ();
      kind  = kind;
      state = BRef.ref UV_UVar;
      scope = BRef.ref scope
    }

  let kind u = u.kind

  let equal u1 u2 = u1 == u2

  let raw_set u tp =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar   ->
      BRef.set u.state (UV_Type tp);
      BRef.get u.scope

  let fix u =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar ->
      let x = TVar.fresh u.kind in
      BRef.set u.state (UV_Type (t_var x));
      x

  let shrink_scope ~scope u =
    BRef.set u.scope (TVar.Set.inter (BRef.get u.scope) scope)

  module Set = Set.Make(Ordered)
end
