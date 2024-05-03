(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for match clauses and related constructs *)

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
  let (cls, _) = check_match_clauses ~tcfix env tp cls res_tp res_eff in
  let x = Var.fresh () in
  let body =
    { T.pos;
      T.data = T.EMatch({ pos; data = T.EVar x }, cls, res_tp, res_eff) }
  in (x, body)

(* ------------------------------------------------------------------------- *)
let check_return_clauses ~tcfix env rcs res_tp res_eff =
  match rcs with
  | [] ->
    let x = Var.fresh () in
    (x, res_tp, { T.pos = Position.nowhere; T.data = T.EVar x })
  | _ ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let (x, body) = make_nonempty_match ~tcfix env tp rcs res_tp res_eff in
    (x, tp, body)

(* ------------------------------------------------------------------------- *)
let check_finally_clauses (type dir)
  ~tcfix env fcs hexpr htp (req : (T.typ, dir) request) eff :
    T.expr * (T.typ, dir) response * ret_effect =
  match fcs with
  | [] ->
    begin match req with
    | Infer -> (hexpr, Infered htp, Impure)
    | Check tp ->
      Error.check_unify_result ~pos:hexpr.pos
        (Unification.subtype env htp tp)
        ~on_error:(Error.expr_type_mismatch ~env htp tp);
      (hexpr, Checked, Impure)
    end
  | _ ->
    let (tp, (resp : (T.typ, dir) response)) =
      match req with
      | Infer ->
        let tp = Env.fresh_uvar env T.Kind.k_type in
        (tp, Infered tp)
      | Check tp -> (tp, Checked)
    in
    let (x, body) = make_nonempty_match ~tcfix env htp fcs tp eff in
    let expr =
      { T.pos  = Position.join body.pos hexpr.pos;
        T.data = T.ELet(x, T.Scheme.of_type htp, hexpr, body) }
    in (expr, resp, Impure)
