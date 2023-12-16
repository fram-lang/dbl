(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on effects *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

type effect_view =
  | EffPure
  | EffUVar of uvar
  | EffVar  of tvar
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
    | EEVar  x -> EffVar  x
    | EEUVar u -> EffUVar u
    end

let view_end eff =
  snd (effect_view eff)

let is_pure eff =
  match effect_view eff with
  | (xs, EEClosed) -> TVar.Set.is_empty xs
  | _ -> false
