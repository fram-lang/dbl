(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking subtyping of types *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

type arrow =
  | Arr_No
  | Arr_Pure   of T.typ * T.typ
  | Arr_Impure of T.typ * T.typ * T.effect

(** Internal exception *)
exception Error

let rec check_subeffect env eff1 eff2 =
  match T.Effect.view eff1 with
  | EffPure    -> ()
  | EffUVar u1 ->
    begin match T.Effect.view eff2 with
    | EffPure -> T.UVar.set u1 T.Effect.pure_row
    | EffUVar u2 when T.UVar.equal u1 u2 -> ()
    | EffUVar _ | EffVar _ ->
      (** TODO: add constraint *)
      T.UVar.set u1 eff2
    end
  | EffVar x1 ->
    begin match T.Effect.view eff2 with
    | EffPure -> raise Error
    | EffUVar u ->
      T.UVar.set u (T.Type.t_var x1)
    | EffVar x2 when T.TVar.equal x1 x2 -> ()
    | EffVar _ -> raise Error
    end

let rec check_subtype env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TUVar u1, TUVar u2 when T.UVar.equal u1 u2 -> ()
  | TUVar u, _ ->
    if T.Type.contains_uvar u tp2 then
      raise Error
    else
      T.UVar.set u tp2
  | _, TUVar u ->
    if T.Type.contains_uvar u tp1 then
      raise Error
    else
      T.UVar.set u tp1

  | TUnit, TUnit -> ()
  | TUnit, (TVar _ | TPureArrow _ | TArrow _) -> raise Error

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, (TUnit | TPureArrow _ | TArrow _) -> raise Error

  | TPureArrow(atp1, vtp1), TPureArrow(atp2, vtp2) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TPureArrow(atp1, vtp1), TArrow(atp2, vtp2, _) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TPureArrow _, (TUnit | TVar _) -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2;
    check_subeffect env eff1 eff2
  | TArrow _, (TUnit | TVar _ | TPureArrow _) -> raise Error

  | TRowPure, _ | _, TRowPure ->
    failwith "Internal kind error"

let subeffect env eff1 eff2 =
  (* TODO: create reference backtracking point *)
  match check_subeffect env eff1 eff2 with
  | ()              -> true
  | exception Error -> false

let subtype env tp1 tp2 =
  (* TODO: create reference backtracking point *)
  match check_subtype env tp1 tp2 with
  | ()              -> true
  | exception Error -> false

let to_arrow env tp =
  match T.Type.view tp with
  | TUnit | TVar _ -> Arr_No
  | TUVar u ->
    let tp1 = T.Type.fresh_uvar T.Kind.k_type in
    let tp2 = T.Type.fresh_uvar T.Kind.k_type in
    let eff = T.Type.fresh_uvar T.Kind.k_effrow in
    T.UVar.set u (T.Type.t_arrow tp1 tp2 eff);
    Arr_Impure(tp1, tp2, eff)
  | TPureArrow(tp1, tp2)  -> Arr_Pure(tp1, tp2)
  | TArrow(tp1, tp2, eff) -> Arr_Impure(tp1, tp2, eff)

  | TRowPure ->
    failwith "Internal kind error"

let from_arrow env tp =
  match T.Type.view tp with
  | TUnit | TVar _ -> Arr_No
  | TUVar u ->
    let tp1 = T.Type.fresh_uvar T.Kind.k_type in
    let tp2 = T.Type.fresh_uvar T.Kind.k_type in
    let eff = T.Type.fresh_uvar T.Kind.k_effrow in
    T.UVar.set u (T.Type.t_arrow tp1 tp2 eff);
    Arr_Impure(tp1, tp2, eff)
  | TPureArrow(tp1, tp2) -> Arr_Pure(tp1, tp2)
  | TArrow(tp1, tp2, eff) -> Arr_Impure(tp1, tp2, eff)

  | TRowPure ->
    failwith "Internal kind error"
