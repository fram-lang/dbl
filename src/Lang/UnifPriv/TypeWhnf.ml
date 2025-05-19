(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Weak head normal form of a type *)

open TypeBase

type neutral_head =
  | NH_UVar of uvar
  | NH_Var  of tvar

type whnf =
  | Whnf_Effect
  | Whnf_Neutral of neutral_head * typ list
      (* Arguments are in reversed order! *)
  | Whnf_Arrow of scheme * typ * effct
  | Whnf_Handler   of tvar * typ * typ * typ
  | Whnf_Label of typ

let rec whnf tp =
  match view tp with
  | TEffect -> Whnf_Effect
  | TUVar u -> Whnf_Neutral(NH_UVar u, [])
  | TVar  x -> Whnf_Neutral(NH_Var  x, [])
  | TArrow(sch, tp, eff) -> Whnf_Arrow(sch, tp, eff)
  | THandler(a, tp, itp, otp) ->
    Whnf_Handler(a, tp, itp, otp)
  | TLabel tp0 -> Whnf_Label tp0
  | TApp(tp1, tp2) ->
    begin match whnf tp1 with
    | Whnf_Neutral(h, args) -> Whnf_Neutral(h, tp2 :: args)

    | Whnf_Effect | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
      failwith "Internal kind error"
    end
  | TAlias(_, tp) -> whnf tp
