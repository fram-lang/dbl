(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

(* Author: Piotr Polesiuk, 2023 *)

open KindBase

type tvar = TVar.t

type uvar = {
  uid   : UID.t;
  kind  : kind;
  state : uvar_state BRef.t
}

and uvar_state =
  | UV_UVar
  | UV_Type of typ

and typ = type_view
and type_view =
  | TUnit
  | TUVar  of uvar
  | TVar   of tvar
  | TArrow of typ * typ

type scheme = {
  sch_tvars : tvar list;
  sch_body  : typ
}

let t_unit = TUnit

let t_uvar u = TUVar u

let t_var x = TVar x

let t_arrow tp1 tp2 = TArrow(tp1, tp2)

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
  | TUnit | TVar _ | TArrow _ -> tp

module UVar = struct
  module Ordered = struct
    type t = uvar
    let compare u1 u2 = UID.compare u1.uid u2.uid
  end
  include Ordered

  let fresh kind =
    { uid   = UID.fresh ()
    ; kind  = kind
    ; state = BRef.ref UV_UVar
    }

  let equal u1 u2 = u1 == u2

  let set u tp =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar   -> BRef.set u.state (UV_Type tp)

  let fix u =
    match BRef.get u.state with
    | UV_Type _ -> assert false
    | UV_UVar ->
      let x = TVar.fresh u.kind in
      BRef.set u.state (UV_Type (t_var x));
      x

  module Set = Set.Make(Ordered)
end
