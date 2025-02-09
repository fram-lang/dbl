(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

open Common

let generalize ~pos ~pp tvs named e (sch : Name.scheme) =
  Uniqueness.check_generalized_types ~pos tvs sch.sch_targs;
  Uniqueness.check_generalized_names ~pos ~pp named sch.sch_named;
  let poly_args =
    List.map (fun (name, x, sch) -> (Name.to_unif name, x, sch)) named in
  let poly_expr =
    { T.pos  = pos;
      T.data = T.EGen(tvs, poly_args, e)
    } in
  let sch_named =
    List.map (fun (name, x, sch) -> (name, T.SchemeExpr.to_scheme sch)) named in
  let sch = {
    T.sch_targs = tvs @ sch.sch_targs;
    T.sch_named =
      List.map (fun (name, sch) -> (Name.to_unif name, sch))
        (sch_named @ sch.sch_named);
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
