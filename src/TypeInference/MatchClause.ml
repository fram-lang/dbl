(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for match clauses and related constructs *)
(*
open Common
open TypeCheckFix

let check_match_clause ~tcfix env tp (cl : S.match_clause) res_tp res_eff =
  let open (val tcfix : TCFix) in
  match cl.data with
  | Clause(pat, body) ->
    let scope = Env.scope env in
    let (env, pat, _, r_eff1) = Pattern.check_type ~env ~scope pat tp in
    let (body, r_eff2) = check_expr_type env body res_tp res_eff in
    (pat, body, ret_effect_join r_eff1 r_eff2)

let check_match_clauses ~tcfix env tp cls res_tp res_eff =
  let (r_eff, cls) = List.fold_left_map
    (fun r_eff1 cl ->
      let (pat, body, r_eff2) =
        check_match_clause ~tcfix env tp cl res_tp res_eff in
      (ret_effect_join r_eff1 r_eff2, (pat, body)))
    Pure
    cls
  in
  (cls, r_eff)

(* ------------------------------------------------------------------------- *)
(** Make pattern-matching expression for given non-empty clause list (the
  meaning of parameters is the same as for [check_match_clauses]). It returns
  fresh variable [x], and match-expression that matches [x] against given list
  of clauses. *)
let make_nonempty_match ~tcfix env tp cls res_tp res_eff =
  let pos =
    match cls with
    | [] -> assert false
    | cl :: cls ->
      let p1 = cl.S.pos in
      let p2 = List.fold_left (fun _ cl -> cl.S.pos) p1 cls in
      Position.join p1 p2
  in
  let (cls, r_eff) = check_match_clauses ~tcfix env tp cls res_tp res_eff in
  let meff = match_effect r_eff res_eff in
  let x = Var.fresh () in
  let body =
    { T.pos;
      T.data = T.EMatch({ pos; data = T.EVar x }, cls, res_tp, meff) }
  in (x, body)

(* ------------------------------------------------------------------------- *)
let default_clause ~pos res1 res2 =
  let x = Var.fresh () in
  (x, res1, { T.pos; T.data = T.EVar x }, res2)

let guess_type (type d) env (req : (_, d) request) : _ * (_, d) response =
  match req with
  | Check tp -> (tp, Checked)
  | Infer    ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    (tp, Infered tp)

let tr_opt_clauses (type md rd) ~tcfix ~pos env
  (mtp_req : (_, md) request) cls (rtp_req : (_, rd) request) eff ~on_error :
    _ * (_, md) response * _ * (_, rd) response =
  match mtp_req, cls, rtp_req with
  | Check tp, [], Infer ->
    default_clause ~pos Checked (Infered tp)
  | Infer, [], Check tp ->
    default_clause ~pos (Infered tp) Checked
  | Infer, [], Infer ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    default_clause ~pos (Infered tp) (Infered tp)
  | Check tp1, [], Check tp2 ->
    Error.check_unify_result ~pos (Unification.subtype env tp1 tp2) ~on_error;
    default_clause ~pos Checked Checked
  | _ ->
    let (mtp, mtp_resp) = guess_type env mtp_req in
    let (rtp, rtp_resp) = guess_type env rtp_req in
    let (x, body) = make_nonempty_match ~tcfix env mtp cls rtp eff in
    (x, mtp_resp, body, rtp_resp)
*)
