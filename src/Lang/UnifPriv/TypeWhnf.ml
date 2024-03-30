(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Weak head normal form of a type *)

open TypeBase

type neutral_head =
  | NH_UVar of TVar.Perm.t * uvar
  | NH_Var  of tvar

type whnf =
  | Whnf_Neutral   of neutral_head * typ list
      (* Arguments are in reversed order! *)
  | Whnf_Effect    of effect
  | Whnf_Effrow    of effrow
  | Whnf_PureArrow of scheme * typ
  | Whnf_Arrow     of scheme * typ * effrow
  | Whnf_Handler   of tvar * typ * typ * effrow
  | Whnf_Label     of effect * typ * effrow

let rec whnf tp =
  match view tp with
  | TUVar(p, u) -> Whnf_Neutral(NH_UVar(p, u), [])
  | TVar x      -> Whnf_Neutral(NH_Var x, [])
  | TEffect _   -> Whnf_Effect tp
  | TEffrow _   -> Whnf_Effrow tp
  | TPureArrow(sch, tp) -> Whnf_PureArrow(sch, tp)
  | TArrow(sch, tp, eff) -> Whnf_Arrow(sch, tp, eff)
  | THandler(a, tp, tp0, eff0) -> Whnf_Handler(a, tp, tp0, eff0)
  | TLabel(eff, tp0, eff0)     -> Whnf_Label(eff, tp0, eff0)
  | TApp(tp1, tp2) ->
    begin match whnf tp1 with
    | Whnf_Neutral(h, args) -> Whnf_Neutral(h, tp2 :: args)

    | Whnf_Effect _ | Whnf_Effrow _ | Whnf_PureArrow _
    | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
      failwith "Internal kind error"
    end
