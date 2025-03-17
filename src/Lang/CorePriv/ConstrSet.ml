(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Set of constraints *)

open TypeBase

(** Effect constraints are represented as maps from effect variables to list
  of their upper bounds. *)
type t = effect list TVar.Map.t

let empty = TVar.Map.empty

let upper_bounds cset x =
  match TVar.Map.find_opt x cset with
  | None      -> []
  | Some effs -> effs

let rec add_simplify cset (eff1 : effect) eff2 =
  match eff1 with
  | TEffPure -> cset
  | TEffJoin(effa, effb) ->
    let cset = add_simplify cset effa eff2 in
    add_simplify cset effb eff2
  | TVar x ->
    TVar.Map.add x (eff2 :: upper_bounds cset x) cset
  | TApp _ ->
    failwith "Internal error: TApp in effect"

let add cset (eff1, eff2) =
  add_simplify cset eff1 eff2

let add_list cset cs =
  List.fold_left add cset cs
