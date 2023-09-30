(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Basic operations on types *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

let view = TypeBase.view

let fresh_uvar kind = t_uvar (UVar.fresh kind)

let rec contains_uvar u tp =
  match view tp with
  | TUnit | TVar _ -> false
  | TUVar u' -> UVar.equal u u'
  | TArrow(tp1, tp2) ->
    contains_uvar u tp1 || contains_uvar u tp2

let rec collect_uvars tp uvs =
  match view tp with
  | TUnit | TVar _ -> uvs
  | TUVar u -> UVar.Set.add u uvs
  | TArrow(tp1, tp2) ->
    collect_uvars tp1 (collect_uvars tp2 uvs)

let collect_scheme_uvars sch uvs =
  collect_uvars sch.sch_body uvs

let uvars tp         = collect_uvars tp UVar.Set.empty
let scheme_uvars sch = collect_scheme_uvars sch UVar.Set.empty
