(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

open KindBase

type tvar  = TVar.t
type scope = Scope.t

type tname =
  | TNAnon
  | TNEffect
  | TNVar of string

type named_tvar = tname * tvar

type name =
  | NLabel
  | NVar         of string
  | NOptionalVar of string
  | NImplicit    of string
  | NMethod      of string

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
and effct = typ
and effrow = typ
and type_view =
  | TUVar      of TVar.Perm.t * uvar
  | TVar       of tvar
  | TEffect    of TVar.Set.t
  | TEffrow    of TVar.Set.t * effrow_end
  | TPureArrow of scheme * typ
  | TArrow     of scheme * typ * effrow
  | THandler   of tvar * typ * typ * effrow * typ * effrow
  | TLabel     of effct * typ * effrow
  | TApp       of typ * typ

and effrow_end =
  | EEClosed
  | EEUVar of TVar.Perm.t * uvar
  | EEVar  of tvar
  | EEApp  of typ * typ

and scheme = {
  sch_targs : named_tvar list;
  sch_named : named_scheme list;
  sch_body  : typ
}

and named_scheme = name * scheme

type ctor_decl = {
  ctor_name        : string;
  ctor_targs       : named_tvar list;
  ctor_named       : named_scheme list;
  ctor_arg_schemes : scheme list
}

let t_uvar p u = TUVar(Scope.shrink_perm_dom (BRef.get u.scope) p, u)

let t_var x = TVar x

let t_effect xs = TEffect xs

let t_effrow xs ee = TEffrow(xs, ee)

let t_closed_effrow xs =  TEffrow(xs, EEClosed)

let t_pure_arrow sch tp2 = TPureArrow(sch, tp2)

let t_arrow sch tp2 eff = TArrow(sch, tp2, eff)

let t_handler a tp itp ieff otp oeff = THandler(a, tp, itp, ieff, otp, oeff)

let t_label eff tp0 eff0 = TLabel(eff, tp0, eff0)

let t_app tp1 tp2 = TApp(tp1, tp2)

let perm_name p n =
  match n with
  | NLabel | NVar _ | NOptionalVar _ | NImplicit _ | NMethod _ -> n

let perm_named_tvar p (n, x) =
  (n, TVar.Perm.apply p x)

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
  | TEffrow(xs, EEUVar(p, u)) ->
    begin match view (TUVar(p, u)) with
    | TUVar(p, u) -> TEffrow(xs, EEUVar(p, u))
    | TVar x  -> TEffrow(xs, EEVar x)
    | TApp(tp1, tp2) -> TEffrow(xs, EEApp(tp1, tp2))
    | TEffrow(ys, ee) -> TEffrow(TVar.Set.union xs ys, ee)

    | TEffect _ | TPureArrow _ | TArrow _ | THandler _ | TLabel _ ->
      failwith "Internal kind error"
    end
  | TEffrow(xs, ee) -> tp

  | TVar _ | TEffect _ | TPureArrow _ | TArrow _ | THandler _
  | TLabel _ | TApp _ -> tp

and perm p tp =
  if TVar.Perm.is_identity p then tp
  else perm_rec p tp

and perm_rec p tp =
  match view tp with
  | TUVar(p', u) ->
    let p = TVar.Perm.compose p p' in
    TUVar(Scope.shrink_perm_dom (BRef.get u.scope) p, u)
  | TVar x -> TVar (TVar.Perm.apply p x)
  | TEffect xs ->
    TEffect (TVar.Perm.map_set p xs)
  | TEffrow(xs, ee) ->
    TEffrow(TVar.Perm.map_set p xs, perm_effrow_end_rec p ee)
  | TPureArrow(sch, tp2) ->
    TPureArrow(perm_scheme_rec p sch, perm_rec p tp2)
  | TArrow(sch, tp2, eff) ->
    TArrow(perm_scheme_rec p sch, perm_rec p tp2, perm_rec p eff)
  | THandler(a, tp, itp, ieff, otp, oeff) ->
    THandler(TVar.Perm.apply p a, perm_rec p tp,
      perm_rec p itp, perm_rec p ieff, perm_rec p otp, perm_rec p oeff)
  | TLabel(eff, tp0, eff0) ->
    TLabel(perm_rec p tp, perm_rec p tp0, perm_rec p eff0)
  | TApp(tp1, tp2) ->
    TApp(perm_rec p tp1, perm_rec p tp2)

and perm_effrow_end_rec p ee =
  match ee with
  | EEClosed -> EEClosed
  | EEUVar(p', u) ->
    let p = TVar.Perm.compose p p' in
    EEUVar(Scope.shrink_perm_dom (BRef.get u.scope) p, u)
  | EEVar x -> EEVar (TVar.Perm.apply p x)
  | EEApp(tp1, tp2) ->
    EEApp(perm_rec p tp1, perm_rec p tp2)

and perm_scheme_rec p sch =
  { sch_targs = List.map (perm_named_tvar p) sch.sch_targs;
    sch_named = List.map (perm_named_scheme_rec p) sch.sch_named;
    sch_body  = perm_rec p sch.sch_body
  }

and perm_named_scheme_rec p (n, sch) =
  (perm_name p n, perm_scheme_rec p sch)

let effect_view eff =
  match view eff with
  | TVar x     -> TVar.Set.singleton x
  | TEffect xs -> xs

  | TUVar _ | TApp _ -> assert false

  | TEffrow _ | TPureArrow _ | TArrow _ | THandler _ | TLabel _ ->
    failwith "Internal kind error"

let effrow_view eff =
  match view eff with
  | TUVar(p, u)    -> (TVar.Set.empty, EEUVar(p, u))
  | TVar  x        -> (TVar.Set.empty, EEVar x)
  | TApp(tp1, tp2) -> (TVar.Set.empty, EEApp(tp1, tp2))

  | TEffrow(xs, ee) -> (xs, ee)

  | TEffect _ | TPureArrow _ | TArrow _ | THandler _ | TLabel _ ->
    failwith "Internal kind error"

module UVar = struct
  module Ordered = struct
    type t = uvar
    let compare u1 u2 = UID.compare u1.uid u2.uid
  end
  include Ordered

  let fresh ~scope kind =
    assert (KindBase.non_effect kind);
    { uid   = UID.fresh ();
      kind  = kind;
      state = BRef.create UV_UVar;
      scope = BRef.create scope
    }

  let kind u = u.kind

  let equal u1 u2 = u1 == u2

  let uid u = u.uid

  let scope u = BRef.get u.scope

  let level u = Scope.level (scope u)

  let raw_set p u tp =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar   ->
      BRef.set u.state (UV_Type (perm (TVar.Perm.inverse p) tp));
      Scope.perm p (BRef.get u.scope)

  let fix u =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar ->
      let x = TVar.fresh u.kind in
      BRef.set u.state (UV_Type (t_var x));
      x

  let filter_scope u lvl f =
    BRef.set u.scope (Scope.filter lvl f (BRef.get u.scope))

  module Set = Set.Make(Ordered)
  module Map = Map.Make(Ordered)
end
