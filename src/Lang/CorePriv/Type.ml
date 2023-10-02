(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on types *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

(** Get the kind of given type *)
let rec kind : type k. k typ -> k kind =
  function
  | TUnit     -> KType
  | TEffPure  -> KEffect
  | TVar    x -> TVar.kind x
  | TArrow  _ -> KType
  | TForall _ -> KType

(** Substitute one type in another *)
let subst_type x stp tp =
  Subst.in_type (Subst.singleton x stp) tp

(** Check if one type is a subtype of another *)
let rec subtype tp1 tp2 =
  match tp1, tp2 with
  | TUnit, TUnit  -> true
  | TUnit, (TVar _ | TArrow _ | TForall _) -> false

  | TVar x, TVar y -> x == y
  | TVar _, (TUnit | TArrow _ | TForall _) -> false

  | TArrow(atp1, vtp1), TArrow(atp2, vtp2) ->
    subtype atp2 atp1 && subtype vtp1 vtp2
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
