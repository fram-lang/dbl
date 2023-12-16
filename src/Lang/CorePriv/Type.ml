(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on types *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

(** Get the kind of given type *)
let rec kind : type k. k typ -> k kind =
  function
  | TUnit      -> KType
  | TEffPure   -> KEffect
  | TEffJoin _ -> KEffect
  | TVar     x -> TVar.kind x
  | TArrow   _ -> KType
  | TForall  _ -> KType

(** Substitute one type in another *)
let subst_type x stp tp =
  Subst.in_type (Subst.singleton x stp) tp

(** Check equality of types *)
let rec equal : type k. k typ -> k typ -> bool =
  fun tp1 tp2 ->
  match tp1, tp2 with
  | TEffPure,   _ -> effect_equal tp1 tp2
  | TEffJoin _, _ -> effect_equal tp1 tp2
  | _, TEffPure   -> effect_equal tp1 tp2
  | _, TEffJoin _ -> effect_equal tp1 tp2

  | TUnit, TUnit -> true
  | TUnit, _     -> false

  | TVar x, TVar y -> TVar.equal x y
  | TVar _, _      -> false

  | TArrow(ta1, tb1, eff1), TArrow(ta2, tb2, eff2) ->
    equal ta1 ta2 && equal tb1 tb2 && effect_equal eff1 eff2
  | TArrow _, _ -> false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      if TVar.equal x1 x2 then equal tp1 tp2
      else begin
        let x = TVar.fresh (TVar.kind x1) in
        equal (subst_type x1 (TVar x) tp1) (subst_type x2 (TVar x) tp2)
      end
    | NotEqual -> false
    end
  | TForall _, _ -> false

(** Check equality of effects *)
and effect_equal : effect -> effect -> bool =
  fun eff1 eff2 -> subeffect eff1 eff2 && subeffect eff2 eff1

(** Check if one effect is a subeffect of another *)
and subeffect eff1 eff2 =
  match eff1 with
  | TEffPure -> true
  | TEffJoin(eff_a, eff_b) ->
    subeffect eff_a eff2 && subeffect eff_b eff2
  | TVar _ -> simple_subeffect eff1 eff2

(** Check if simple effect (different than pure and join) is a subeffect of
  another effect *)
and simple_subeffect eff1 eff2 =
  match eff2 with
  | TEffPure -> false
  | TEffJoin(eff_a, eff_b) ->
    simple_subeffect eff1 eff_a || simple_subeffect eff2 eff_b
  | TVar _ -> equal eff1 eff2

(** Check if one type is a subtype of another *)
let rec subtype tp1 tp2 =
  match tp1, tp2 with
  | TUnit, TUnit  -> true
  | TUnit, (TVar _ | TArrow _ | TForall _) -> false

  | TVar x, TVar y -> x == y
  | TVar _, (TUnit | TArrow _ | TForall _) -> false

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    subtype atp2 atp1 && subtype vtp1 vtp2 && subeffect eff1 eff2
  | TArrow _, (TUnit | TVar _ | TForall _) -> false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    (* TODO: it can be done better than O(n^2) time. *)
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      let x = TVar (TVar.clone x1) in
      subtype (subst_type x1 x tp1) (subst_type x2 x tp2)
    | NotEqual -> false
    end
  | TForall _, (TUnit | TVar _ | TArrow _) -> false

type ex = Ex : 'k typ -> ex
