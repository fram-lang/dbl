(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type erasure for ConE programs *)

module S = Lang.ConE
module T = Lang.Untyped

(** Expression-building do-notation *)
let (let^ ) m cont = m cont
let (let* ) m f cont = m (Fun.flip f cont)
let return x cont = cont x

let mk_let e cont =
  let x = Var.fresh () in
  T.ELet(x, e, cont x)

(** Translate a data or label definition *)
let tr_data_def (dd : S.data_def) e =
  match dd with
  | DD_Data _ -> e
  | DD_Label lbl -> T.ELabel(lbl.var, e)

(** Translate expression *)
let rec tr_expr (e : S.expr) =
  match e with
  | EUnitPrf | EOptionPrf -> assert false

  | ENum _ | ENum64 _ | EStr _ | EChr _ | EVar _ | EFn _ | ECtor _
  | EExtern _ ->
    let^ v = tr_expr_v e in
    T.EValue v

  | ETFun(_, body) | ECAbs(_, body) | ETApp(body, _) | ECApp body
  | ERecCtx body ->
    tr_expr body

  | EApp(e1, e2) ->
    let^ v1 = tr_expr_v e1 in
    let^ v2 = tr_expr_v e2 in
    T.EApp(v1, v2)

  | ELet(x, e1, e2) | ELetPure(x, e1, e2) ->
    T.ELet(x, tr_expr e1, tr_expr e2)

  | ELetRec(rds, e) ->
    T.ELetRec(List.map tr_rec_def rds, tr_expr e)

  | EData(dds, e) ->
    List.fold_right tr_data_def dds (tr_expr e)

  | EMatch(_, e, cls, _, _) ->
    let^ v = tr_expr_v e in
    T.EMatch(v, List.map tr_clause cls)

  | EShift(lbl_e, x, body, _) ->
    let^ lbl_v = tr_expr_v lbl_e in
    T.EShift(lbl_v, [], x, tr_expr body)

  | EReset(lbl_e, body, ret_var, ret_body) ->
    let^ lbl_v = tr_expr_v lbl_e in
    T.EReset(lbl_v, [], tr_expr body, ret_var, tr_expr ret_body)

  | ERepl(func, _, _) ->
    T.ERepl (fun () -> tr_expr (func ()))

  | EReplExpr(e1, tp, e2) ->
    T.EReplExpr(tr_expr e1, tp, tr_expr e2)

(** Translate expression as a value *)
and tr_expr_v (e : S.expr) =
  match e with
  | EUnitPrf | EOptionPrf -> assert false

  | ENum   n        -> return (T.VNum n)
  | ENum64 n        -> return (T.VNum64 n)
  | EStr   s        -> return (T.VStr s)
  | EChr   c        -> return (T.VNum (Char.code c))
  | EVar   x        -> return (T.VVar x)
  | EFn(x, _, body) -> return (T.VFn(x, tr_expr body))

  | ECtor(_, idx, _, args) ->
    let* args = tr_expr_vs args in
    return (T.VCtor(idx, args))

  | EExtern(name, _) -> return (T.VExtern name)

  | ETFun(_, body) | ECAbs(_, body) | ETApp(body, _) | ECApp body
  | ERecCtx body ->
    tr_expr_v body

  | EApp _ | ELet _ | ELetPure _ | ELetRec _ | EData _ | EMatch _ | EShift _ 
  | EReset _ | ERepl _ | EReplExpr _ ->
    let* x = mk_let (tr_expr e) in
    return (T.VVar x)

(** Translate a list of expressions as list of values in expression building
  monad. *)
and tr_expr_vs es =
  match es with
  | [] -> return []
  | e :: es ->
    let* v = tr_expr_v e in
    let* vs = tr_expr_vs es in
    return (v :: vs)

(** Translate a recursive definition *)
and tr_rec_def (rd : S.rec_def) =
  (rd.rd_var, tr_expr rd.rd_body)

(** Translate a clause of pattern-matching *)
and tr_clause (cl : S.match_clause) =
  (cl.cl_vars, tr_expr cl.cl_body)

let tr_program p =
  tr_expr p
