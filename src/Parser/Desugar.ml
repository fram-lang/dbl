(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The first phase of desugaring and post-parsing *)

(* Author: Piotr Polesiuk, 2023 *)

open Lang.Surface

let rec tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit        -> make EUnit
  | EParen e     -> make (tr_expr e).data
  | EVar x       -> make (EVar x)
  | EFn(x, e)    -> make (EFn(x, tr_expr e))
  | EApp(e1, e2) -> make (EApp(tr_expr e1, tr_expr e2))
  | EDefs(defs, e) ->
    let defs = tr_defs defs in
    defs (tr_expr e)
  | EHandle(x, e, h) ->
    make (EHandle(x, tr_expr e, tr_h_expr h))

and tr_h_expr (h : Raw.h_expr) =
  let make data = { h with data = data } in
  match h.data with
  | HEffect(x, r, e) ->
    make (HEffect(x, r, tr_expr e))

and tr_def (def : Raw.def) =
  let make f (rest : expr) =
    { pos  = Position.join def.pos rest.pos
    ; data = f rest
    }
  in
  match def.data with
  | DLet(x, e) ->
    let e = tr_expr e in
    if is_value e then
      make (fun rest -> ELetV(x, e, rest))
    else
      make (fun rest -> ELetE(x, e, rest))

and tr_defs defs =
  match defs with
  | []          -> fun rest -> rest
  | def :: defs ->
    let def  = tr_def def   in
    let defs = tr_defs defs in
    fun rest -> def (defs rest)

let tr_program (p : Raw.program) =
  tr_defs p.data { pos = p.pos; data = EUnit }
