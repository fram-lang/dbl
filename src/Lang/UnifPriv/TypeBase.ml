(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

open UnifCommon
include Names

type kind = Kind.t

type tvar  = TVar.t

type named_tvar = tname * tvar

type effct = Pure | Impure

type uvar = {
  uid   : UID.t;
  kind  : kind;
  state : uvar_state BRef.t;
  scope : Scope.t BRef.t
}

and uvar_state =
  | UV_UVar
  | UV_Type of typ

and typ = type_view
and type_view =
  | TEffect
  | TUVar    of uvar
  | TVar     of tvar
  | TArrow   of scheme * typ * effct
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

let t_uvar u = TUVar u

let t_var x = TVar x

let t_arrow sch tp2 eff = TArrow(sch, tp2, eff)

let t_handler a tp itp otp = THandler(a, tp, itp, otp)

let t_label tp0 = TLabel tp0

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
  | TEffect | TVar _ | TArrow _ | THandler _ | TLabel _ | TApp _ -> tp

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
      let x = TVar.fresh ~scope:(BRef.get u.scope) u.kind in
      BRef.set u.state (UV_Type (t_var x));
      x

  let shrink_scope u scope =
    BRef.set u.scope (Scope.inter (BRef.get u.scope) scope)

  module Set = Set.Make(Ordered)
  module Map = Map.Make(Ordered)
end
