(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for match clauses and related constructs *)

open Common
open TypeCheckFix

let check_match_clause ~tcfix env tp (cl : S.match_clause) res_tp =
  let open (val tcfix : TCFix) in
  match cl.data with
  | Clause(pat, body) ->
    let (env, pat, eff) = Pattern.check_type_ext env pat tp in
    let er = check_expr_type env body res_tp in
    (pat, er.er_expr, T.Effect.join eff er.er_effect, er.er_constr)

let check_match_clauses ~tcfix env tp cls res_tp =
  let check (eff1, cs1) cl =
    let (pat, body, eff2, cs2) = check_match_clause ~tcfix env tp cl res_tp in
    let eff = T.Effect.join eff1 eff2 in
    let cs = cs1 @ cs2 in
    ((eff, cs), (pat, body))
  in
  let ((eff, cs), cls) = List.fold_left_map check (T.Pure, []) cls in
  (cls, eff, cs)

let guess_type (type d) env (req : (_, d) request) : _ * (_, d) response =
  match req with
  | Check tp -> (tp, Checked)
  | Infer    ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    (tp, Infered tp)

let tr_opt_clauses (type dir) ~tcfix ~pos env tp_in cls
    (rtp_req : (_, dir) request) ~(on_error : pos:_ -> _) :
    _ * dir expr_result =
  let x = Var.fresh () in
  let make data = { T.pos; data } in
  let x_expr = make (T.EInst(make (T.EVar x), [], [])) in
  match cls, rtp_req with
  | [], Infer ->
    let er =
      { er_expr   = x_expr;
        er_type   = Infered tp_in;
        er_effect = T.Pure;
        er_constr = []
      }
    in (x, er)

  | [], Check tp_out ->
    Error.check_unify_result ~pos (Unification.subtype env tp_in tp_out)
      ~on_error:(on_error tp_in tp_out);
    let er =
      { er_expr   = x_expr;
        er_type   = Checked;
        er_effect = T.Pure;
        er_constr = []
      }
    in (x, er)

  | _ :: _, _ ->
    let (tp, tp_resp) = guess_type env rtp_req in
    let (cls, eff, cs) = check_match_clauses ~tcfix env tp_in cls tp in
    let er =
      { er_expr   = make (T.EMatch(x_expr, cls, tp, eff));
        er_type   = tp_resp;
        er_effect = eff;
        er_constr = cs
      }
    in (x, er)

let tr_return_clauses ~tcfix ~pos env tp_in cls rtp_req =
  tr_opt_clauses ~tcfix ~pos env tp_in cls rtp_req
    ~on_error:(Error.return_type_mismatch ~env)

let tr_finally_clauses ~tcfix ~pos env tp_in cls rtp_req =
  tr_opt_clauses ~tcfix ~pos env tp_in cls rtp_req
    ~on_error:(Error.finally_type_mismatch ~env)
