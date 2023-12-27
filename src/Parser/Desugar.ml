(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The first phase of desugaring and post-parsing *)

(* Author: Piotr Polesiuk, 2023 *)

open Lang.Surface

let rec tr_type_expr (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TWildcard -> make TWildcard
  | TVar x    -> make (TVar x)
  | TPureArrow(tp1, tp2) ->
    make (TPureArrow(tr_type_expr tp1, tr_type_expr tp2))
  | TArrow(tp1, tp2, eff) ->
    make (TArrow(tr_type_expr tp1, tr_type_expr tp2, tr_type_expr eff))
  | TEffect(tps, ee) ->
    make (TEffect(List.map tr_type_expr tps, Option.map tr_type_expr ee))

let tr_pattern (p : Raw.pattern) =
  let make data = { p with data = data } in
  match p.data with
  | PWildcard -> make PWildcard
  | PVar  x   -> make (PVar x)
  | PName n   -> make (PName n)

let rec tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit          -> make EUnit
  | EParen e       -> make (tr_expr e).data
  | EVar x         -> make (EVar x)
  | EName n        -> make (EName n)
  | ECtor c        -> make (ECtor c)
  | EFn(x, e)      -> make (EFn(x, tr_expr e))
  | EApp(e1, e2)   -> make (EApp(tr_expr e1, tr_expr e2))
  | EDefs(defs, e) -> make (EDefs(tr_defs defs, tr_expr e))
  | EMatch(e, cls) -> make (EMatch(tr_expr e, List.map tr_match_clause cls))
  | EHandle(x, e, h) ->
    make (EHandle(x, tr_expr e, tr_h_expr h))

and tr_match_clause (cl : Raw.match_clause) =
  let make data = { cl with data = data } in
  match cl.data with
  | Clause(pat, body) ->
    make (Clause(tr_pattern pat, tr_expr body))

and tr_h_expr (h : Raw.h_expr) =
  let make data = { h with data = data } in
  match h.data with
  | HEffect(x, r, e) ->
    make (HEffect(x, r, tr_expr e))

and tr_def (def : Raw.def) =
  let make data = { def with data = data } in
  match def.data with
  | DLet({ data = PWildcard; _}, e) ->
    (* TODO: patterns in let-expressions *)
    make (DLet("_", tr_expr e))
  | DLet({ data = PVar x; _}, e)  -> make (DLet(x, tr_expr e))
  | DLet({ data = PName n; _}, e) -> make (DLetName(n, tr_expr e))
  | DImplicit n    -> make (DImplicit n)
  | DData(x, cs)   -> make (DData(x, List.map tr_ctor_decl cs))

and tr_defs defs = List.map tr_def defs

and tr_ctor_decl (d : Raw.ctor_decl) =
  let make data = { d with data = data } in
  match d.data with
  | CtorDecl(name, tps) -> make (CtorDecl(name, List.map tr_type_expr tps))

let tr_program (p : Raw.program) =
  let make data = { p with data = data } in
  make (EDefs(tr_defs p.data, make EUnit))
