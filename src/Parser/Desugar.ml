(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The first phase of desugaring and post-parsing *)

(* Author: Piotr Polesiuk, 2023 *)

open Lang.Surface

let rec tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit          -> make EUnit
  | EParen e       -> make (tr_expr e).data
  | EVar x         -> make (EVar x)
  | EName n        -> make (EName n)
  | EFn(x, e)      -> make (EFn(x, tr_expr e))
  | EApp(e1, e2)   -> make (EApp(tr_expr e1, tr_expr e2))
  | EDefs(defs, e) -> make (EDefs(tr_defs defs, tr_expr e))
  | EHandle(x, e, h) ->
    make (EHandle(x, tr_expr e, tr_h_expr h))

and tr_h_expr (h : Raw.h_expr) =
  let make data = { h with data = data } in
  match h.data with
  | HEffect(x, r, e) ->
    make (HEffect(x, r, tr_expr e))

and tr_def (def : Raw.def) =
  let make data = { def with data = data } in
  match def.data with
  | DLet(x, e)     -> make (DLet(x, tr_expr e))
  | DLetName(n, e) -> make (DLetName(n, tr_expr e))
  | DImplicit n    -> make (DImplicit n)

and tr_defs defs = List.map tr_def defs

let tr_program (p : Raw.program) =
  let make data = { p with data = data } in
  make (EDefs(tr_defs p.data, make EUnit))
