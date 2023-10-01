(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

module Sub = TVar.Map.Make(struct type 'k t = 'k typ end)

type t = Sub.t

let singleton = Sub.singleton

let is_empty = Sub.is_empty

let add_tvar sub x =
  let y = TVar.clone x in
  (Sub.add x (TVar y) sub, y)

let rec in_type_rec : type k. t -> k typ -> k typ =
  fun sub tp ->
  match tp with
  | TUnit  -> TUnit
  | TVar x ->
    begin match Sub.find_opt x sub with
    | None    -> tp
    | Some tp -> tp
    end
  | TArrow(tp1, tp2) ->
    TArrow(in_type_rec sub tp1, in_type_rec sub tp2)
  | TForall(x, tp) ->
    let (sub, x) = add_tvar sub x in
    TForall(x, in_type_rec sub tp)

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp
