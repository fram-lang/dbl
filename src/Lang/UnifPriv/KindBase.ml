(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(** The state of kind unification variable *)
type kuvar_state =
  | KS_Any
    (** This unification variable can be concretized to any kind *)

  | KS_NonEffect
    (** This unification variable cannot have an effect kind *)

type kuvar = (kuvar_state, kind) Either.t BRef.t
and kind = kind_view
and kind_view =
  | KType
  | KEffect
  | KEffrow
  | KUVar  of kuvar
  | KArrow of kind * kind

let k_type = KType

let k_effect = KEffect
let k_effrow = KEffrow

let rec view k =
  match k with
  | KType | KEffect | KEffrow | KArrow _ -> k
  | KUVar u ->
    begin match BRef.get u with
    | Left _ -> k
    | Right k ->
      let k = view k in
      BRef.set u (Right k);
      k
    end

let non_effect k =
  match view k with
  | KType | KEffrow | KArrow _ -> true
  | KEffect -> false
  | KUVar u ->
    begin match BRef.get u with
    | Left KS_Any       -> false
    | Left KS_NonEffect -> true
    | Right _ -> assert false
    end

let is_effect k =
  match view k with
  | KEffect -> true
  | _ -> false

let set_non_effect k =
  match view k with
  | KType | KEffrow | KArrow _ -> true
  | KEffect -> false
  | KUVar u ->
    begin match BRef.get u with
    | Left KS_Any ->
      BRef.set u (Left KS_NonEffect);
      true
    | Left KS_NonEffect -> true
    | Right _ -> assert false
    end

module KUVar = struct
  let fresh ~non_effect () =
    BRef.create (Either.Left (if non_effect then KS_NonEffect else KS_Any))

  let equal x y = x == y

  let set x k =
    match BRef.get x with
    | Either.Left KS_Any ->
      BRef.set x (Right k); true
    | Either.Left KS_NonEffect ->
      (** TODO: set backtracking point? *)
      if set_non_effect k then
        (BRef.set x (Right k); true)
      else
        false
    | Either.Right _ -> assert false

  let set_safe x k =
    let r = set x k in
    assert r
end

let fresh_uvar ?(non_effect=false) () = KUVar (KUVar.fresh ~non_effect ())

let k_arrow k1 k2 =
  assert (non_effect k2);
  KArrow(k1, k2)

let k_arrows ks k = List.fold_right k_arrow ks k

let rec contains_uvar x k =
  match view k with
  | KType | KEffect | KEffrow -> false
  | KUVar u -> KUVar.equal x u
  | KArrow(k1, k2) ->
    contains_uvar x k1 || contains_uvar x k2
