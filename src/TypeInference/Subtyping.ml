(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking subtyping of types *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Internal exception *)
exception Error

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
  | TUnit, (TVar _ | TArrow _) -> raise Error

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, (TUnit | TArrow _) -> raise Error

  | TArrow(atp1, vtp1), TArrow(atp2, vtp2) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TArrow _, (TUnit | TVar _) -> raise Error

let subtype env tp1 tp2 =
  (* TODO: create reference backtracking point *)
  match check_subtype env tp1 tp2 with
  | ()              -> true
  | exception Error -> false

let to_arrow env tp =
  match T.Type.view tp with
  | TUnit | TVar _ -> None
  | TUVar u ->
    let tp1 = T.Type.fresh_uvar T.Kind.k_type in
    let tp2 = T.Type.fresh_uvar T.Kind.k_type in
    T.UVar.set u (T.Type.t_arrow tp1 tp2);
    Some(tp1, tp2)
  | TArrow(tp1, tp2) -> Some(tp1, tp2)

let from_arrow env tp =
  match T.Type.view tp with
  | TUnit | TVar _ -> None
  | TUVar u ->
    let tp1 = T.Type.fresh_uvar T.Kind.k_type in
    let tp2 = T.Type.fresh_uvar T.Kind.k_type in
    T.UVar.set u (T.Type.t_arrow tp1 tp2);
    Some(tp1, tp2)
  | TArrow(tp1, tp2) -> Some(tp1, tp2)
