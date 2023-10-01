(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type erasure. *)

(* Author: Piotr Polesiuk, 2023 *)

module S = Lang.Core
module T = Lang.Untyped

(** Translate expression *)
let rec tr_expr (e : S.expr) =
  match e with
  | EValue v        -> tr_value v
  | ELet(x, e1, e2) | ELetPure(x, e1, e2) ->
    T.ELet(x, tr_expr e1, tr_expr e2)
  | EApp(v1, v2) ->
    tr_value_v v1 (fun v1 ->
    tr_value_v v2 (fun v2 ->
    T.EApp(v1, v2)))
  | ETApp(v, _) -> tr_value v

(** Translate value as an expression *)
and tr_value (v : S.value) =
  match v with
  | VUnit | VVar _ | VFn _ ->
    tr_value_v v (fun v -> T.EValue v)
  | VTFun(_, body) ->
    tr_expr body

(** Translate value as a value, and pass it to given meta-continuation *)
and tr_value_v (v : S.value) cont =
  match v with
  | VUnit  -> cont T.VUnit
  | VVar x -> cont (T.VVar x)
  | VFn(x, _, body) -> cont (T.VFn(x, tr_expr body))
  | VTFun(_, body) ->
    let x = Var.fresh () in
    T.ELet(x, tr_expr body, cont (T.VVar x))

let tr_program p =
  tr_expr p
