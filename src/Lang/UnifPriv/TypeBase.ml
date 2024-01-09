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
  | TUVar      of TVar.Perm.t * uvar
  | TVar       of tvar
  | TEffect    of TVar.Set.t * effect_end
  | TPureArrow of scheme * typ
  | TArrow     of scheme * typ * effect
  | TApp       of typ * typ

and effect_end =
  | EEClosed
  | EEUVar of TVar.Perm.t * uvar
  | EEVar  of tvar
  | EEApp  of typ * typ

and scheme = {
  sch_tvars    : tvar list;
  sch_implicit : (name * typ) list;
  sch_body     : typ
}

type ctor_decl = {
  ctor_name      : string;
  ctor_arg_types : typ list
}

let t_unit = TUnit

let t_uvar p u = TUVar(TVar.Perm.shrink_dom (BRef.get u.scope) p, u)

let t_var x = TVar x

let t_effect xs ee = TEffect(xs, ee)

let t_closed_effect xs =  TEffect(xs, EEClosed)

let t_pure_arrow sch tp2 = TPureArrow(sch, tp2)

let t_arrow sch tp2 eff = TArrow(sch, tp2, eff)

let t_app tp1 tp2 = TApp(tp1, tp2)

let rec view tp =
  match tp with
  | TUVar(p, u) ->
    begin match BRef.get u.state with
    | UV_UVar -> tp
    | UV_Type tp ->
      (* Path compression *)
      let tp = view tp in
      BRef.set u.state (UV_Type tp);
      perm p tp
    end
  | TEffect(xs, EEUVar(p, u)) ->
    begin match view (TUVar(p, u)) with
    | TUVar(p, u) -> TEffect(xs, EEUVar(p, u))
    | TVar x  -> TEffect(xs, EEVar x)
    | TApp(tp1, tp2) -> TEffect(xs, EEApp(tp1, tp2))
    | TEffect(ys, ee) -> TEffect(TVar.Set.union xs ys, ee)

    | TUnit | TPureArrow _ | TArrow _ ->
      failwith "Internal kind error"
    end
  | TEffect(xs, ee) -> tp

  | TUnit | TVar _ | TPureArrow _ | TArrow _ | TApp _ -> tp

and perm p tp =
  if TVar.Perm.is_identity p then tp
  else perm_rec p tp

and perm_rec p tp =
  match view tp with
  | TUnit -> TUnit
  | TUVar(p', u) ->
    let p = TVar.Perm.compose p p' in
    TUVar(TVar.Perm.shrink_dom (BRef.get u.scope) p, u)
  | TVar x -> TVar (TVar.Perm.apply p x)
  | TEffect(xs, ee) ->
    TEffect(TVar.Perm.map_set p xs, perm_effect_end_rec p ee)
  | TPureArrow(sch, tp2) ->
    TPureArrow(perm_scheme_rec p sch, perm_rec p tp2)
  | TArrow(sch, tp2, eff) ->
    TArrow(perm_scheme_rec p sch, perm_rec p tp2, perm_rec p eff)
  | TApp(tp1, tp2) ->
    TApp(perm_rec p tp1, perm_rec p tp2)

and perm_effect_end_rec p ee =
  match ee with
  | EEClosed -> EEClosed
  | EEUVar(p', u) ->
    let p = TVar.Perm.compose p p' in
    EEUVar(TVar.Perm.shrink_dom (BRef.get u.scope) p, u)
  | EEVar x -> EEVar (TVar.Perm.apply p x)
  | EEApp(tp1, tp2) ->
    EEApp(perm_rec p tp1, perm_rec p tp2)

and perm_scheme_rec p sch =
  { sch_tvars    = List.map (TVar.Perm.apply p) sch.sch_tvars;
    sch_implicit =
      List.map (fun (name, tp) -> (name, perm_rec p tp)) sch.sch_implicit;
    sch_body     = perm_rec p sch.sch_body
  }

let effect_view eff =
  match view eff with
  | TUVar(p, u) -> (TVar.Set.empty, EEUVar(p, u))
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

  let raw_set p u tp =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar   ->
      BRef.set u.state (UV_Type (perm (TVar.Perm.inverse p) tp));
      TVar.Perm.map_set p (BRef.get u.scope)

  let fix u =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar ->
      let x = TVar.fresh u.kind in
      BRef.set u.state (UV_Type (t_var x));
      x

  let shrink_scope ~scope u =
    BRef.set u.scope (TVar.Set.inter (BRef.get u.scope) scope)

  let filter_scope u f =
    BRef.set u.scope (TVar.Set.filter f (BRef.get u.scope))

  module Set = Set.Make(Ordered)
end
