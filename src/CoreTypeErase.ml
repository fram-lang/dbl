(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type erasure for Core programs *)

module S = Lang.Core
module T = Lang.Untyped

(** Translate a data or label definition *)
let tr_data_def (dd : S.data_def) e =
  match dd with
  | DD_Data _ -> e
  | DD_Label lbl -> T.ELabel(lbl.var, e)

(** Translate a literal *)
let tr_lit (l : S.lit) =
  match l with
  | LNum   n -> T.LNum n
  | LNum64 n -> T.LNum64 n
  | LStr   s -> T.LStr s

(** Translate expression *)
let rec tr_expr (e : S.expr) =
  match e with
  | EValue v -> T.EValue (tr_value v)
  | ELet(x, e1, e2) | ELetPure(Relevant, x, e1, e2) ->
    T.ELet(x, tr_expr e1, tr_expr e2)

  | ELetPure(Irrelevant, _, _, e) | ERecCtx e | ETFun(_, e) | ECAbs(_, e)
  | ETApp(e, _) | ECApp e ->
    tr_expr e

  | ELetRec(rds, e) ->
    T.ELetRec(
      List.map (fun (x, _, e) -> (x, tr_expr e)) rds,
      tr_expr e)

  | EFn(x, _, body) -> T.EFn(x, tr_expr body)
  | EApp(e1, v2) -> T.EApp(tr_expr e1, tr_value v2)
  | EData(dds, e) -> List.fold_right tr_data_def dds (tr_expr e)
  | ECtor(_, n, _, args) ->
    T.ECtor(n, List.map tr_value args)
  | EMatch(_, v, cls, _, _) ->
    T.EMatch(tr_value v, List.map tr_clause cls)
  | EShift(v, _, xs, x, e, _) ->
    T.EShift(tr_value v, xs, x, tr_expr e)
  | EReset(v, _, vs, body, x, ret) ->
    T.EReset(tr_value v, List.map tr_value vs, tr_expr body, x, tr_expr ret)
  | ERepl(func, _, _) ->
    T.ERepl (fun () -> tr_expr (func ()))
  | EReplExpr(e1, tp, e2) ->
    T.EReplExpr(tr_expr e1, tp, tr_expr e2)

(** Translate value as a value *)
and tr_value (v : S.value) =
  match v with
  | VLit l -> T.VLit (tr_lit l)
  | VVar x -> T.VVar x
  | VExtern(name, _) -> T.VExtern name

(** Translate a clause of pattern-matching *)
and tr_clause (cl : S.match_clause) =
  (cl.cl_vars, tr_expr cl.cl_body)

let tr_program p =
  tr_expr p
