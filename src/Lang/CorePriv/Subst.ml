(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

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
  | TUnit | TEffPure -> tp
  | TEffJoin(eff1, eff2) ->
    TEffJoin(in_type_rec sub eff1, in_type_rec sub eff2)
  | TVar x ->
    begin match Sub.find_opt x sub with
    | None    -> tp
    | Some tp -> tp
    end
  | TArrow(tp1, tp2, eff) ->
    TArrow(in_type_rec sub tp1, in_type_rec sub tp2, in_type_rec sub eff)
  | TForall(x, tp) ->
    let (sub, x) = add_tvar sub x in
    TForall(x, in_type_rec sub tp)
  | TData(tp, ctors) ->
    TData(in_type_rec sub tp, List.map (in_ctor_type_rec sub) ctors)
  | TApp(tp1, tp2) ->
    TApp(in_type_rec sub tp1, in_type_rec sub tp2)

and in_ctor_type_rec sub ctor =
  { ctor_name      = ctor.ctor_name;
    ctor_arg_types = List.map (in_type_rec sub) ctor.ctor_arg_types
  }

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp
