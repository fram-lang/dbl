(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type t = typ TVar.Map.t

let empty = TVar.Map.empty

let add_type sub x tp =
  TVar.Map.add x tp sub

let is_empty sub =
  TVar.Map.is_empty sub

(* TODO: write a bit about how substitution in effects is handled *)
let in_effvar sub x ys =
  match TVar.Map.find_opt x sub with
  | None     -> TVar.Set.add x ys
  | Some eff ->
    begin match view eff with
    | TVar x ->
      assert (KindBase.view (TVar.kind x) = KClEffect);
      TVar.Set.add x ys
    | TEffect(xs, EEClosed) -> TVar.Set.union xs ys

    | TEffect(_, (EEUVar _ | EEVar _ | EEApp _)) | TUVar _ | TApp _ ->
      (* Substitution of non-closed effect for variable of kind cleffect *)
      assert false

    | TUnit | TPureArrow _ | TArrow _ ->
      failwith "Internal kind error"
    end

let rec in_effect_end sub ee =
  match ee with
  | EEClosed | EEUVar _ -> (TVar.Set.empty, ee)
  | EEVar x  ->
    begin match TVar.Map.find_opt x sub with
    | None     -> (TVar.Set.empty, ee)
    | Some eff -> effect_view eff
    end
  | EEApp(tp1, tp2) ->
    (TVar.Set.empty, EEApp(in_type_rec sub tp1, in_type_rec sub tp2))

and in_type_rec sub tp =
  match TypeBase.view tp with
  | TUnit | TUVar _ -> tp
  | TVar x ->
    begin match TVar.Map.find_opt x sub with
    | None    -> tp
    | Some tp -> tp
    end
  | TEffect(xs, ee) ->
    let (ys, ee) = in_effect_end sub ee in
    t_effect (TVar.Set.fold (in_effvar sub) xs ys) ee
  | TPureArrow(tp1, tp2) ->
    t_pure_arrow (in_type_rec sub tp1) (in_type_rec sub tp2)
  | TArrow(tp1, tp2, eff) ->
    t_arrow (in_type_rec sub tp1) (in_type_rec sub tp2) (in_type_rec sub eff)
  | TApp(tp1, tp2) ->
    t_app (in_type_rec sub tp1) (in_type_rec sub tp2)

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp

let in_ctor_decl sub ctor =
  if is_empty sub then ctor
  else {
    ctor_name      = ctor.ctor_name;
    ctor_arg_types = List.map (in_type_rec sub) ctor.ctor_arg_types
  }
