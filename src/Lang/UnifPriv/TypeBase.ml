(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

open KindBase

type tvar  = TVar.t
type scope = Scope.t

type tname =
  | TNAnon
  | TNVar of string

type named_tvar = tname * tvar

type name =
  | NVar         of string
  | NOptionalVar of string
  | NImplicit    of string
  | NMethod      of string

type effect = Pure | Impure

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
and type_view =
  | TEffect
  | TUVar    of TVar.Perm.t * uvar
  | TVar     of tvar
  | TArrow   of scheme * typ * effect
  | THandler of tvar * typ * typ * typ
  | TLabel   of typ
  | TApp     of typ * typ

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

let t_effect = TEffect

let t_uvar p u = TUVar(Scope.shrink_perm_dom (BRef.get u.scope) p, u)

let t_var x = TVar x

let t_arrow sch tp2 eff = TArrow(sch, tp2, eff)

let t_handler a tp itp otp = THandler(a, tp, itp, otp)

let t_label tp0 = TLabel tp0

let t_app tp1 tp2 = TApp(tp1, tp2)

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
  | TEffect | TVar _ | TArrow _ | THandler _ | TLabel _ | TApp _ -> tp

and perm p tp =
  if TVar.Perm.is_identity p then tp
  else perm_rec p tp

and perm_rec p tp =
  match view tp with
  | TEffect -> TEffect
  | TUVar(p', u) ->
    let p = TVar.Perm.compose p p' in
    TUVar(Scope.shrink_perm_dom (BRef.get u.scope) p, u)
  | TVar x -> TVar (TVar.Perm.apply p x)
  | TArrow(sch, tp2, eff) ->
    TArrow(perm_scheme_rec p sch, perm_rec p tp2, eff)
  | THandler(a, tp, itp, otp) ->
    THandler(TVar.Perm.apply p a, perm_rec p tp,
      perm_rec p itp, perm_rec p otp)
  | TLabel tp0 -> TLabel (perm_rec p tp0)
  | TApp(tp1, tp2) ->
    TApp(perm_rec p tp1, perm_rec p tp2)

and perm_scheme_rec p sch =
  { sch_targs = List.map (perm_named_tvar p) sch.sch_targs;
    sch_named = List.map (perm_named_scheme_rec p) sch.sch_named;
    sch_body  = perm_rec p sch.sch_body
  }

and perm_named_scheme_rec p (n, sch) =
  (n, perm_scheme_rec p sch)

module UVar = struct
  module Ordered = struct
    type t = uvar
    let compare u1 u2 = UID.compare u1.uid u2.uid
  end
  include Ordered

  let fresh ~scope kind =
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
