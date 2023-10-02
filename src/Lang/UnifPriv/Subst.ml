(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

type t = typ TVar.Map.t

let empty = TVar.Map.empty

let add_type sub x tp =
  TVar.Map.add x tp sub

let is_empty sub =
  TVar.Map.is_empty sub

let rec in_type_rec sub tp =
  match TypeBase.view tp with
  | TUnit | TRowPure | TUVar _ -> tp
  | TVar x ->
    begin match TVar.Map.find_opt x sub with
    | None    -> tp
    | Some tp -> tp
    end
  | TPureArrow(tp1, tp2) ->
    t_pure_arrow (in_type_rec sub tp1) (in_type_rec sub tp2)
  | TArrow(tp1, tp2, eff) ->
    t_arrow (in_type_rec sub tp1) (in_type_rec sub tp2) (in_type_rec sub eff)

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp
