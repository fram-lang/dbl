(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Basic operations on types *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

let view = TypeBase.view

let fresh_uvar ~scope kind = t_uvar (UVar.fresh ~scope kind)

let t_pure_arrows tps tp = List.fold_right t_pure_arrow tps tp

let open_effect_up ~scope eff =
  let (xs, ee) = effect_view eff in
  match ee with
  | EEClosed -> t_effect xs (EEUVar (UVar.fresh ~scope KindBase.k_effect))
  | EEVar _ | EEUVar _ -> eff

let rec open_down ~scope tp =
  match view tp with
  | TUnit | TUVar _ | TVar _ -> tp
  | TPureArrow(tp1, tp2) ->
    t_pure_arrow (open_up ~scope tp1) (open_down ~scope tp2)
  | TArrow(tp1, tp2, eff) ->
    t_arrow (open_up ~scope tp1) (open_down ~scope tp2) eff

  | TEffect _ ->
    failwith "Internal kind error"

and open_up ~scope tp =
  match view tp with
  | TUnit | TUVar _ | TVar _ -> tp
  | TPureArrow(tp1, tp2) ->
    t_pure_arrow (open_down ~scope tp1) (open_up ~scope tp2)
  | TArrow(tp1, tp2, eff) ->
    t_arrow
      (open_down      ~scope tp1)
      (open_up        ~scope tp2)
      (open_effect_up ~scope eff)

  | TEffect _ ->
    failwith "Internal kind error"

let rec contains_uvar u tp =
  match view tp with
  | TUnit | TVar _ | TEffect(_, (EEClosed | EEVar _)) -> false
  | TUVar u' | TEffect(_, EEUVar u') -> UVar.equal u u'
  | TPureArrow(tp1, tp2) ->
    contains_uvar u tp1 || contains_uvar u tp2
  | TArrow(tp1, tp2, eff) ->
    contains_uvar u tp1 || contains_uvar u tp2 || contains_uvar u eff

let rec collect_uvars tp uvs =
  match view tp with
  | TUnit | TVar _ | TEffect(_, (EEClosed | EEVar _)) -> uvs
  | TUVar u | TEffect(_, EEUVar u) -> UVar.Set.add u uvs
  | TPureArrow(tp1, tp2) ->
    collect_uvars tp1 (collect_uvars tp2 uvs)
  | TArrow(tp1, tp2, eff) ->
    collect_uvars tp1 (collect_uvars tp2 (collect_uvars eff uvs))

let collect_scheme_uvars sch uvs =
  collect_uvars sch.sch_body uvs

let uvars tp         = collect_uvars tp UVar.Set.empty
let scheme_uvars sch = collect_scheme_uvars sch UVar.Set.empty

(* ========================================================================= *)

exception Escapes_scope of tvar

let shrink_var_scope ~scope x =
  if Scope.mem scope x then ()
  else raise (Escapes_scope x)

let shrink_effect_end_scope ~scope ee =
  match ee with
  | EEClosed -> ()
  | EEVar  x -> shrink_var_scope ~scope x
  | EEUVar u -> UVar.shrink_scope ~scope u

let rec shrink_scope ~scope tp =
  match view tp with
  | TUnit -> ()
  | TUVar u -> UVar.shrink_scope ~scope u
  | TVar  x -> shrink_var_scope  ~scope x
  | TEffect(xs, ee) ->
    TVar.Set.iter (shrink_var_scope ~scope) xs;
    shrink_effect_end_scope ~scope ee
  | TPureArrow(tp1, tp2) ->
    shrink_scope ~scope tp1;
    shrink_scope ~scope tp2
  | TArrow(tp1, tp2, eff) ->
    shrink_scope ~scope tp1;
    shrink_scope ~scope tp2;
    shrink_scope ~scope eff

let try_shrink_scope ~scope tp =
  (* TODO: set backtracking point *)
  match shrink_scope ~scope tp with
  | () -> Ok ()
  | exception Escapes_scope x -> Error x
