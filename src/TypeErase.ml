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

(** Translate expression *)
let rec tr_expr (e : S.expr) =
  match e with
  | EValue v -> tr_value v
  | ELet(x, e1, e2) | ELetPure(x, e1, e2) ->
    T.ELet(x, tr_expr e1, tr_expr e2)
  | ELetIrr(_, _, e) -> tr_expr e
  | ELetRec(rds, e) ->
    T.ELetRec(
      List.map (fun (x, _, e) -> (x, tr_expr e)) rds,
      tr_expr e)
  | ERecCtx e -> tr_expr e
  | EApp(v1, v2) ->
    tr_value_v v1 (fun v1 ->
    tr_value_v v2 (fun v2 ->
    T.EApp(v1, v2)))
  | ETApp(v, _) | ECApp v -> tr_value v
  | EData(dds, e) -> List.fold_right tr_data_def dds (tr_expr e)
  | EMatch(_, v, cls, _, _) ->
    tr_value_v v (fun v ->
    T.EMatch(v, List.map tr_clause cls))
  | EShift(v, _, xs, x, e, _) ->
    tr_value_v v (fun v ->
    T.EShift(v, xs, x, tr_expr e))
  | EReset(v, _, vs, body, x, ret) ->
    tr_value_v v (fun v ->
    tr_value_vs vs (fun vs ->
    T.EReset(v, vs, tr_expr body, x, tr_expr ret)))
  | ERepl(func, _, _) ->
    T.ERepl (fun () -> tr_expr (func ()))
  | EReplExpr(e1, tp, e2) ->
    T.EReplExpr(tr_expr e1, tp, tr_expr e2)

(** Translate value as an expression *)
and tr_value (v : S.value) =
  match v with
  | VNum _ | VNum64 _ | VStr _ | VVar _ | VFn _ | VCtor _ | VExtern _ ->
    tr_value_v v (fun v -> T.EValue v)
  | VTFun(_, body) | VCAbs(_, body) ->
    tr_expr body

(** Translate value as a value, and pass it to given meta-continuation *)
and tr_value_v (v : S.value) cont =
  match v with
  | VNum n   -> cont (T.VNum n)
  | VNum64 n -> cont (T.VNum64 n)
  | VStr s   -> cont (T.VStr s)
  | VVar x   -> cont (T.VVar x)
  | VFn(x, _, body) -> cont (T.VFn(x, tr_expr body))
  | VTFun(_, EValue v) | VCAbs(_, EValue v) -> tr_value_v v cont
  | VTFun(_, body) | VCAbs(_, body) ->
    let x = Var.fresh () in
    T.ELet(x, tr_expr body, cont (T.VVar x))
  | VCtor(_, n, _, args) ->
    tr_value_vs args (fun args ->
    cont (T.VCtor(n, args)))
  | VExtern(name, _) -> cont (T.VExtern name)

(** Translate list of values and pass the result (list of values) to given
  meta-continuation *)
and tr_value_vs vs cont =
  match vs with
  | [] -> cont []
  | v :: vs ->
    tr_value_v  v  (fun v ->
    tr_value_vs vs (fun vs ->
    cont (v :: vs)))

(** Translate a clause of pattern-matching *)
and tr_clause (cl : S.match_clause) =
  (cl.cl_vars, tr_expr cl.cl_body)

let tr_program p =
  tr_expr p
