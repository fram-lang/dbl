(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on effects *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type effect_view =
  | EffPure
  | EffUVar of uvar
  | EffVar  of tvar
  | EffApp  of typ * typ
  | EffCons of tvar * effect

let pure = t_closed_effect TVar.Set.empty

let singleton x =
  t_closed_effect (TVar.Set.singleton x)

let io = pure (* TODO *)

let cons x eff =
  let (xs, ee) = effect_view eff in
  t_effect (TVar.Set.add x xs) ee

let view eff =
  let (xs, ee) = effect_view eff in
  match TVar.Set.choose_opt xs with
  | Some x -> EffCons(x, t_effect (TVar.Set.remove x xs) ee)
  | None ->
    begin match ee with
    | EEClosed -> EffPure
    | EEUVar u -> EffUVar u
    | EEVar  x -> EffVar  x
    | EEApp(tp1, tp2) -> EffApp(tp1, tp2)
    end

let view_end eff =
  snd (effect_view eff)

let is_pure eff =
  match effect_view eff with
  | (xs, EEClosed) -> TVar.Set.is_empty xs
  | _ -> false
