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

(** Check if given effect is pure *)
let rec is_pure_effect eff =
  match eff with
  | TEffPure             -> true
  | TEffJoin(eff1, eff2) -> is_pure_effect eff1 && is_pure_effect eff2
  | TVar _               -> false

(** Check if effect row [eff] contains effect variable [x]. In other words,
  it checks if [x] is a subeffect of [eff]. *)
let rec contains_effect_var x eff =
  match eff with
  | TEffPure             -> false
  | TEffJoin(eff1, eff2) ->
    contains_effect_var x eff1 || contains_effect_var x eff2
  | TVar y -> x == y

(** Check if one effect is a subeffect of another *)
let rec subeffect eff1 eff2 =
  match eff1 with
  | TEffPure               -> true
  | TEffJoin(eff_a, eff_b) -> subeffect eff_a eff2 && subeffect eff_b eff2
  | TVar x                 -> contains_effect_var x eff2

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

(** Join effect variable with another effect. *)
let rec eff_cons_var x eff =
  if contains_effect_var x eff then
    eff
  else if is_pure_effect eff then
    TVar x
  else
    TEffJoin(TVar x, eff)

(** Join two effects *)
let rec eff_join eff1 eff2 =
  match eff1 with
  | TEffPure               -> eff2
  | TEffJoin(eff_a, eff_b) -> eff_join eff_a (eff_join eff_b eff2)
  | TVar x                 -> eff_cons_var x eff2

type ex = Ex : 'k typ -> ex
