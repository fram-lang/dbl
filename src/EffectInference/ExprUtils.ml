(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions for expressions. *)

open Common

let mk_unit = T.ECtor(T.EUnitPrf, 0, [], [])

let mk_fn (x, sch) e = T.EFn(x, sch, e)

let mk_fns xs e = List.fold_right mk_fn xs e

let mk_named_fn (_, x, sch) e = T.EFn(x, sch, e)

let mk_named_fns xs e = List.fold_right mk_named_fn xs e

let mk_tfun x e = T.ETFun(x, e)

let mk_tfuns tvs e = List.fold_right mk_tfun tvs e

let mk_named_tfun (_, x) e = T.ETFun(x, e)

let mk_named_tfuns tvs e = List.fold_right mk_named_tfun tvs e

let rec mk_named_tfuns' targs cont =
  match targs with
  | [] -> cont []
  | (_, x) :: targs ->
    let y = T.TVar.clone ~scope:Scope.any x in
    T.ETFun(y, mk_named_tfuns' targs (fun ys -> cont (y :: ys)))

let mk_app e1 e2 = T.EApp(e1, e2)

let mk_apps e es = List.fold_left mk_app e es

let mk_tapp e tp = T.ETApp(e, tp)

let mk_tapps e tps = List.fold_left mk_tapp e tps

let mk_inst e tps es = mk_apps (mk_tapps e tps) es

let mk_linst e tps = T.ECApp(mk_tapps e tps)

let mk_poly_fun targs named e =
  mk_named_tfuns targs (mk_named_fns named e)

(* ========================================================================= *)

let generalize targs named (e : T.expr) (esch : T.scheme) =
  let named_schs = List.map (fun (name, _, sch) -> (name, sch)) named in
  let sch =
    { T.sch_targs = targs @ esch.sch_targs;
      T.sch_named = named_schs @ esch.sch_named;
      T.sch_body  = esch.sch_body
    } in
  let e =
    match named, esch.sch_targs with
    | _, [] -> mk_poly_fun targs named e
    | [], _ -> mk_named_tfuns targs e
    | _, targs2 ->
      let targs2 =
        List.map (fun (_, x) -> T.TVar.clone ~scope:Scope.any x) targs2 in
      let tps2 = List.map T.Type.t_var targs2 in
      mk_named_tfuns targs
        (mk_tfuns targs2 (mk_named_fns named (mk_tapps e tps2)))
  in
  (e, sch)

let generalize_constr evs cs e =
  mk_tfuns evs (T.ECAbs(cs, e))

let mk_ctor ~prf ~idx tp (ctors : T.ctor_decl list) =
  let ctor = List.nth ctors idx in
  let sch =
    { T.sch_targs = ctor.ctor_targs;
      T.sch_named = ctor.ctor_named;
      T.sch_body  = T.Type.t_pure_arrows ctor.ctor_arg_schemes tp
    } in
  let named =
    List.map (fun (name, sch) -> (name, Var.fresh (), sch)) ctor.ctor_named in
  let vars  =
    List.map (fun sch -> (Var.fresh (), sch)) ctor.ctor_arg_schemes in
  let tps = List.map (fun (_, x) -> T.Type.t_var x) ctor.ctor_targs in
  let es1 = List.map (fun (_, x, _) -> T.EVar x) named in
  let es2 = List.map (fun (x, _) -> T.EVar x) vars in
  let e = T.ECtor(prf, idx, tps, es1 @ es2) in
  let e = mk_named_tfuns sch.sch_targs (mk_named_fns named (mk_fns vars e)) in
  (e, sch)

let mk_clause_body tvs xs e =
  let unit_sch = T.Scheme.of_type (T.Type.t_var T.BuiltinType.tv_unit) in
  mk_tfuns tvs (mk_fns xs (T.EFn(Var.fresh (), unit_sch, e)))

let mk_handle eff_var cap_x cap_tp e1 e2 =
  T.EApp(e1,
    T.ETFun(eff_var,
      T.EFn(cap_x, T.Scheme.of_type cap_tp, e2)))

let mk_handler
    ~eff_var ~lbl_var ~delim_tp ~delim_eff ~cap_tp ~in_tp ~in_eff 
    ~cap_body ~ret_var ~ret_body ~fin_var ~fin_body () =
  let comp_var = Var.fresh ~name:"comp" () in
  let sch =
    { T.sch_targs = [(TNAnon, eff_var)];
      T.sch_named = [];
      T.sch_body  =
        T.Type.t_arrow (T.Scheme.of_type cap_tp) in_tp (Impure in_eff)
    } in
  let label_dd =
    T.DD_Label
      { tvar      = eff_var;
        var       = lbl_var;
        delim_tp  = delim_tp;
        delim_eff = delim_eff
      }
  in
  T.EFn(comp_var, sch,
    T.EData([label_dd],
      T.ELet(fin_var,
        T.EReset(T.EVar lbl_var,
          T.EApp(T.ETApp(T.EVar comp_var, T.Type.t_var eff_var), cap_body),
          ret_var, ret_body),
        fin_body)))

(* ========================================================================= *)

type rec_ctx = (T.var * T.expr) list

let mk_rec_ctx_def ~evs ~cs ~named ~targs (x, x_poly, sch) =
  let tps  = List.map (fun (_, x) -> T.Type.t_var x) targs in
  let effs = List.map T.Type.t_var evs in
  let es   = List.map (fun (_, x, _) -> T.EVar x) named in
  (x, mk_named_tfuns' sch.T.sch_targs (fun xs ->
    let tps = tps @ List.map T.Type.t_var xs in
    mk_apps (mk_tapps (mk_linst (T.EVar x_poly) effs) tps) es))

let mk_rec_ctx ~evs ~cs ~targs ~named all_defs =
  List.map (mk_rec_ctx_def ~evs ~cs ~targs ~named) all_defs

let rec update_rec_body ~rec_ctx (e : T.expr) : T.expr =
  match e with
  | EUnitPrf | EOptionPrf | ENum _ | ENum64 _ | EStr _ | EChr _ | EExtern _ ->
    e
  
  | EVar x ->
    assert (List.for_all (fun (y, _) -> not (Var.equal x y)) rec_ctx);
    e

  | EFn(x, sch, body) -> EFn(x, sch, update_rec_body ~rec_ctx body)
  | ETFun(x, body) -> ETFun(x, update_rec_body ~rec_ctx body)
  | ECAbs(cs, body) -> ECAbs(cs, update_rec_body ~rec_ctx body)
  | EApp(e1, e2) ->
    EApp(update_rec_body ~rec_ctx e1, update_rec_body ~rec_ctx e2)
  | ETApp(e1, tp) -> ETApp(update_rec_body ~rec_ctx e1, tp)
  | ECApp e1 -> ECApp(update_rec_body ~rec_ctx e1)
  | ELet(x, e1, e2) ->
    ELet(x, update_rec_body ~rec_ctx e1, update_rec_body ~rec_ctx e2)
  | ELetPure(x, e1, e2) ->
    ELetPure(x, update_rec_body ~rec_ctx e1, update_rec_body ~rec_ctx e2)

  | ELetRec(defs, e) ->
    ELetRec(
      List.map (update_rec_def_rec_body ~rec_ctx) defs,
      update_rec_body ~rec_ctx e)

  | ERecCtx e ->
    ERecCtx (
      List.fold_right (fun (x, e1) e2 -> T.ELetPure(x, e1, e2)) rec_ctx e)

  | EData(dds, e) -> EData(dds, update_rec_body ~rec_ctx e)

  | ECtor(prf, idx, tps, es) ->
    ECtor(
      update_rec_body ~rec_ctx prf,
      idx,
      tps,
      List.map (update_rec_body ~rec_ctx) es)

  | EMatch(prf, e, cls, tp, eff) ->
    EMatch(
      update_rec_body ~rec_ctx prf,
      update_rec_body ~rec_ctx e,
      List.map (update_clause_rec_body ~rec_ctx) cls,
      tp, eff)

  | EShift(lbl, k, body, tp) ->
    EShift(update_rec_body ~rec_ctx lbl, k, update_rec_body ~rec_ctx body, tp)

  | EReset(lbl, body, x, ret) ->
    EReset(
      update_rec_body ~rec_ctx lbl,
      update_rec_body ~rec_ctx body,
      x,
      update_rec_body ~rec_ctx ret)

  | ERepl _ | EReplExpr _ ->
    assert false

and update_rec_def_rec_body ~rec_ctx (rd : T.rec_def) : T.rec_def =
  { rd with rd_body = update_rec_body ~rec_ctx rd.rd_body }

and update_clause_rec_body ~rec_ctx (cl : T.match_clause) : T.match_clause =
  { cl with cl_body = update_rec_body ~rec_ctx cl.cl_body }
