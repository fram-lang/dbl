(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Weak head normal form of a type *)

(* Author: Piotr Polesiuk, 2024 *)

open TypeBase

type neutral_head =
  | NH_UVar of TVar.Perm.t * uvar
  | NH_Var  of tvar

type whnf =
  | Whnf_Unit
  | Whnf_Neutral   of neutral_head * typ list
      (* Arguments are in reversed order! *)
  | Whnf_Effect    of effect
  | Whnf_PureArrow of scheme * typ
  | Whnf_Arrow     of scheme * typ * effect

let rec whnf tp =
  match view tp with
  | TUnit       -> Whnf_Unit
  | TUVar(p, u) -> Whnf_Neutral(NH_UVar(p, u), [])
  | TVar x      -> Whnf_Neutral(NH_Var x, [])
  | TEffect _   -> Whnf_Effect tp
  | TPureArrow(sch, tp) -> Whnf_PureArrow(sch, tp)
  | TArrow(sch, tp, eff) -> Whnf_Arrow(sch, tp, eff)
  | TApp(tp1, tp2) ->
    begin match whnf tp1 with
    | Whnf_Neutral(h, args) -> Whnf_Neutral(h, tp2 :: args)

    | Whnf_Unit | Whnf_Effect _ | Whnf_PureArrow _ | Whnf_Arrow _ ->
      failwith "Internal kind error"
    end
