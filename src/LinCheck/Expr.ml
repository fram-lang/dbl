(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Transforming expressions. *)

open Lang.Unif

let process_data_def env dd =
  match dd with
  | DD_Data  _ -> env
  | DD_Label l -> Env.add_var env l.var TAny

let rec process_pattern env pat tp =
  match pat.data with
  | PWildcard -> env

  | PAs(pat, x) ->
    let env = process_pattern env pat tp in
    Env.add_var env x tp

  | PCtor(name, idx, prf, tvs, pats1, pats2) ->
    let env = process_patterns env pats1 in
    process_patterns env pats2

  | PAnnot(pat, _) ->
    process_pattern env pat tp

  | POr(pat1, _) ->
    (* Subpatterns bind the same variables, so we can just process the first
      one. *)
    process_pattern env pat1 TAny

and process_patterns env pats =
  List.fold_left (fun env pat -> process_pattern env pat TAny) env pats

let rec tr_poly_expr env pe =
  let make data = { pe with data } in
  match pe.data with
  | EVar x -> (pe, Env.lookup_var env x)

  | ECtor(_, _, _) -> (pe, TAny)

  | EPolyFun(tvs, args, body) ->
    let env = Env.add_vars env (List.map (fun (_, x, _) -> x) args) in
    let (body, tp, rs) = tr_expr env body in
    assert (Resource.is_none rs);
    let res = make (EPolyFun(tvs, args, body)) in
    (res, tp)

  | EGen(tvs, args, body) ->
    let env = Env.add_vars env (List.map (fun (_, x, _) -> x) args) in
    let (body, tp) = tr_poly_expr env body in
    let res = make (EGen(tvs, args, body)) in
    (res, tp)

and tr_poly_fun env pf =
  let make data = { pf with data } in
  match pf.data with
  | PF_Fun(tvs, xs, body) ->
    let env = Env.add_vars env xs in
    let (body, tp, rs) = tr_expr env body in
    Resource.ensure_no_resources_t body tp;
    assert (Resource.is_none rs);
    make (PF_Fun(tvs, xs, body))

  | PF_Hole hole ->
    begin match BRef.get hole with
    | Some pf -> tr_poly_fun env pf
    | None    ->
      (* Method constraints should be already solved. *)
      assert false
    end

and tr_expr env e =
  let make data = { e with data } in
  match e.data with
  | EInst(pe, targs, args) ->
    let (pe, tp) = tr_poly_expr env pe in
    let args = List.map (tr_poly_fun env) args in
    let res = make (EInst(pe, targs, args)) in
    (res, tp, RNone)

  | ENum _ | ENum64 _ | EStr _ | EChr _ | EExtern _ ->
    (e, TAny, RNone)

  | EFn(x, se, body, eff) ->
    let env = Env.add_var env x TAny in
    let (body, tp, rs) = tr_expr env body in
    let res = make (EFn(x, se, body, eff)) in
    (res, Resource.arrow tp rs, RNone)

  | EAppPoly(e1, e2) ->
    let (e1, tp1, rs1) = tr_expr env e1 in
    let e2 = tr_poly_fun env e2 in
    let (tp2, rs2) = Resource.as_arrow ~pos:e.pos tp1 in
    Resource.ensure_no_resources_t e2 tp2;
    let e1 = Resource.guard e1 rs2 in
    let res = make (EAppPoly(e1, e2)) in
    (res, tp2, Resource.seq_join rs1 rs2)

  | EAppMono(e1, e2) ->
    let (e1, tp1, rs1) = tr_expr env e1 in
    let (e2, tp2, rs2) = tr_expr env e2 in
    Resource.ensure_no_resources_t e2 tp2;
    let (tp3, rs3) = Resource.as_arrow ~pos:e.pos tp1 in
    let rs23 = Resource.seq_join rs2 rs3 in
    let e1 = Resource.guard e1 rs23 in
    let e2 = Resource.guard e2 rs3  in
    let res = make (EAppMono(e1, e2)) in
    (res, tp3, Resource.seq_join rs1 rs23)

  | ELetPoly(x, pe, rest) ->
    let (pe, tp1) = tr_poly_expr env pe in
    let env = Env.add_var env x tp1 in
    let (rest, tp2, rs) = tr_expr env rest in
    let res = make (ELetPoly(x, pe, rest)) in
    (res, tp2, rs)

  | ELetMono(x, e1, rest) ->
    let (e1, tp1, rs1) = tr_expr env e1 in
    let env = Env.add_var env x tp1 in
    let (rest, tp2, rs2) = tr_expr env rest in
    let e1 = Resource.guard e1 rs2 in
    let res = make (ELetMono(x, e1, rest)) in
    (res, tp2, Resource.seq_join rs1 rs2)

  | ELetRec { targs; named; defs; body } ->
    let defs = tr_rec_defs env named defs in
    let env  = Env.add_vars env (List.map (fun rd -> rd.rd_poly_var) defs) in
    let (body, tp, rs) = tr_expr env body in
    let res = make (ELetRec { targs; named; defs; body }) in
    (res, tp, rs)

  | ERecCtx e ->
    let env = Env.commit_rec_context env in
    let (e, tp, rs) = tr_expr env e in
    let res = make (ERecCtx e) in
    (res, tp, rs)

  | EData(dds, rest) ->
    let env = List.fold_left process_data_def env dds in
    let (rest, tp, rs) = tr_expr env rest in
    let res = make (EData(dds, rest)) in
    (res, tp, rs)

  | ETypeAlias(a, tp_expr, e) ->
    let (e, tp, rs) = tr_expr env e in
    let res = make (ETypeAlias(a, tp_expr, e)) in
    (res, tp, rs)

  | EMatchEmpty(prf, e, tp, eff) ->
    let (e, tp1, rs1) = tr_expr env e in
    let res = make (EMatchEmpty(prf, e, tp, eff)) in
    (res, tp1, rs1)

  | EMatch(e, cls, tp, eff) ->
    let (e, tp1, rs1) = tr_expr env e in
    let (cls, tp2, rs2) = tr_match_clauses env cls tp1 in
    let e = Resource.guard e rs2 in
    let res = make (EMatch(e, cls, tp, eff)) in
    (res, tp2, Resource.seq_join rs1 rs2)

  | EMatchPoly(pe, pat, rest, tp, eff) ->
    let (pe, tp1) = tr_poly_expr env pe in
    let env = process_pattern env pat tp1 in
    let (rest, tp2, rs) = tr_expr env rest in
    let res = make (EMatchPoly(pe, pat, rest, tp, eff)) in
    (res, tp2, rs)

  | EHandle(a, x, e1, e2) ->
    let (e1, tp1, rs1) = tr_expr env e1 in
    let env = Env.add_var env x TAny in
    let (e2, tp2, rs2) = tr_expr env e2 in
    Resource.ensure_no_resources_t e2 tp2;
    Resource.ensure_no_resources_r e2 rs2;
    let res = make (EHandle(a, x, e1, e2)) in
    (res, TAny, rs1)

  | EHandler h ->
    let env = Env.add_var env h.label TAny in
    let (cap_body, cap_tp, cap_rs) = tr_expr env h.cap_body in
    Resource.ensure_no_resources_t h.cap_body cap_tp;
    Resource.ensure_no_resources_r h.cap_body cap_rs;
    let ret_env = Env.add_var env h.ret_var TAny in
    let (ret_body, ret_tp, ret_rs) = tr_expr ret_env h.ret_body in
    Resource.ensure_no_resources_t h.ret_body ret_tp;
    Resource.ensure_no_resources_r h.ret_body ret_rs;
    let fin_env = Env.add_var env h.fin_var TAny in
    let (fin_body, fin_tp, fin_rs) = tr_expr fin_env h.fin_body in
    Resource.ensure_no_resources_t h.fin_body fin_tp;
    Resource.ensure_no_resources_r h.fin_body fin_rs;
    let res = make (EHandler { h with cap_body; ret_body; fin_body }) in
    (res, TAny, RNone)

  | EHandlerFn h ->
    let env = Env.add_var env h.comp_var TAny in
    let (body, tp, rs) = tr_expr env h.body in
    Resource.ensure_no_resources_t h.body tp;
    Resource.ensure_no_resources_r h.body rs;
    let res = make (EHandlerFn { h with body }) in
    (res, TAny, RNone)

  | EEffect(lbl,
      (({ mode = Unrestricted; _ } | { should_check = false; _ }) as mode),
      x, body, res_tp) ->
    let (lbl, tp1, rs1) = tr_expr env lbl in
    let env = Env.add_var env x TAny in
    let (body, tp2, rs2) = tr_expr env body in
    Resource.ensure_no_resources_t body tp2;
    Resource.ensure_no_resources_r body rs2;
    let res = make (EEffect(lbl, mode, x, body, res_tp)) in
    (res, TAny, rs1)

  | EEffect(lbl, ({ mode = Affine; _ } as mode), x, body, res_tp) ->
    let (lbl, tp1, rs1) = tr_expr env lbl in
    let rsid = Resource.fresh ~pos:e.pos Affine in
    let env = Env.add_var env x (TArrow(TAny, RSome(rsid, []))) in
    let (body, tp2, rs2) = tr_expr env body in
    Resource.ensure_no_resources_t body tp2;
    Resource.check_usage body rsid rs2;
    let res = make (EEffect(lbl, mode, x, body, res_tp)) in
    (res, TAny, rs1)

  | EAnnot(e, tp_expr) ->
    let (e, tp, rs) = tr_expr env e in
    let res = make (EAnnot(e, tp_expr)) in
    (res, tp, rs)

  | EAnnotEff(e, tp_expr, eff_expr) ->
    let (e, tp, rs) = tr_expr env e in
    let res = make (EAnnotEff(e, tp_expr, eff_expr)) in
    (res, tp, rs)

  | EModeAnnot a ->
    let (body, tp, rs) = tr_expr env a.body in
    let res = make (EModeAnnot { a with body }) in
    (res, tp, rs)

  | ERepl(f, tp_expr) ->
    let f () =
      let e = f () in
      let (res, tp, rs) = tr_expr env e in
      Resource.ensure_no_resources_t e tp;
      Resource.ensure_no_resources_r e rs;
      res
    in
    let res = make (ERepl(f, tp_expr)) in
    (res, TAny, RNone)

  | EReplExpr { body; to_str; rest } ->
    let (body, tp1, rs1) = tr_expr env body in
    Resource.ensure_no_resources_t body tp1;
    Resource.ensure_no_resources_r body rs1;
    let (to_str, tp2, rs2) = tr_expr env to_str in
    Resource.ensure_no_resources_t to_str tp2;
    Resource.ensure_no_resources_r to_str rs2;
    let (rest, tp3, rs3) = tr_expr env rest in
    Resource.ensure_no_resources_t rest tp3;
    Resource.ensure_no_resources_r rest rs3;
    let res = make (EReplExpr { body; to_str; rest }) in
    (res, TAny, RNone)

and tr_rec_defs env named defs =
  let env = Env.add_vars env (List.map (fun (_, x, _) -> x) named) in
  let env = Env.enter_rec_context env in
  let env = Env.add_rec_vars env (List.map (fun rd -> rd.rd_var) defs) in
  List.map (fun rd -> { rd with rd_body = tr_poly_fun env rd.rd_body }) defs

and tr_match_clauses env cls matched_type =
  let tr_cl (tp, rs) cl =
    let (cl, cl_tp, cl_rs) = tr_match_clause env cl matched_type in
    ((Resource.join_types tp cl_tp, Resource.par_join rs cl_rs), cl)
  in
  match cls with
  | [] -> assert false
  | cl :: cls ->
    let (cl, tp, rs) = tr_match_clause env cl matched_type in
    let ((tp, rs), cls) = List.fold_left_map tr_cl (tp, rs) cls in
    (cl :: cls, tp, rs)

and tr_match_clause env (pat, body) matched_type =
  let env = process_pattern env pat matched_type in
  let (body, tp, rs) = tr_expr env body in
  ((pat, body), tp, rs)
