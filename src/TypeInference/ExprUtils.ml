(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

open Common

(** Add scheme-expression annotations (as pattern-matching) to the expression.
  Returns a list of named arguments, where scheme-expressions are translated
  to schemes, and the extended expression. *)
let annotate_named_args named body res_tp =
  let annotate (name, x, sch_expr) (named, body) =
    let sch = T.SchemeExpr.to_scheme sch_expr in
    let body =
      { body with
        T.data = T.EMatchPoly(
          make_nowhere (T.EVar x),
          make_nowhere (T.PAnnot(
            make_nowhere (T.PAs(make_nowhere T.PWildcard, x)),
            sch_expr)),
          body, res_tp, T.Pure)
      } in
    ((name, x, sch) :: named, body)
  in
  List.fold_right annotate named ([], body)

let generalize ~pos ~pp tvs named e (sch : Name.scheme) =
  Uniqueness.check_generalized_types ~pos tvs sch.sch_targs;
  Uniqueness.check_generalized_names ~pos ~pp named sch.sch_named;
  let inst_tps   =
    List.map (fun (_, x) -> make_nowhere (T.TE_Type (T.Type.t_var x)))
      sch.sch_targs in
  let named_args =
    List.map (fun (name, sch) -> (name, Var.fresh (), sch)) sch.sch_named in
  let inst_named =
    List.map (fun (_, x, _) -> make_nowhere (T.EVar x)) named_args in
  let body = { e with T.data = T.EInst(e, inst_tps, inst_named) } in
  let (named, body) = annotate_named_args named body sch.sch_body in
  let tvs = tvs @ sch.sch_targs in
  let named = named @ named_args in
  let poly_args =
    List.map (fun (name, x, sch) -> (Name.to_unif name, x, sch)) named in
  let poly_expr =
    { e with T.data = T.EPolyFun(tvs, poly_args, body) } in
  let sch = {
    T.sch_targs = tvs;
    T.sch_named =
      List.map (fun (name, _, sch) -> (Name.to_unif name, sch)) named;
    T.sch_body  = sch.sch_body
  } in
  (poly_expr, sch)

(* ========================================================================= *)

let ctor_func ~pos idx (info : Module.adt_info) =
  let make data = { T.pos; T.data } in
  make (T.ECtor(info.adt_args, info.adt_proof, idx))

(* ========================================================================= *)

let match_var pat body tp eff =
  let x = Var.fresh () in
  let e =
    { body with
      T.data = T.EMatchPoly({pat with T.data = T.EVar x}, pat, body, tp, eff)
    } in
  (x, e)

let rec match_args pats body tp eff =
  match pats with
  | [] -> body
  | (x, pat) :: pats ->
    { body with
      T.data = T.EMatchPoly({ pat with T.data = T.EVar x }, pat,
        match_args pats body tp eff, tp, eff)
    }
