(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Variable renaming. *)

open Syntax

type t =
  { scope    : Scope.t;
    tvar_map : TVar.t TVar.Map.t;
    var_map  : Var.t Var.Map.t;
  }

let empty ~scope =
  { scope    = scope;
    tvar_map = TVar.Map.empty;
    var_map  = Var.Map.empty;
  }

let to_subst ren =
  TVar.Map.fold
    (fun x y sub -> Subst.rename_tvar sub x y)
    ren.tvar_map
    (Subst.empty ~scope:ren.scope)

let add_tvar ren x y =
  { ren with tvar_map = TVar.Map.add x y ren.tvar_map }

let add_var ren x y =
  { ren with var_map = Var.Map.add x y ren.var_map }

let rename_tvar ren x =
  match TVar.Map.find_opt x ren.tvar_map with
  | Some y -> y
  | None   -> x

let rename_named_tvar ren (name, x) =
  (name, rename_tvar ren x)

let rename_named_tvars ren xs =
  List.map (rename_named_tvar ren) xs

let rename_type ren tp =
  Subst.in_type (to_subst ren) tp

let rename_scheme ren sch =
  Subst.in_scheme (to_subst ren) sch

let rename_type_expr ren (tp : type_expr) =
  TypeExpr.subst (to_subst ren) tp

let rename_scheme_expr ren (sch : scheme_expr) =
  TypeExpr.subst_in_scheme (to_subst ren) sch

let rename_var ren x =
  match Var.Map.find_opt x ren.var_map with
  | Some y -> y
  | None   -> x

let rec rename_pattern ren (pat : pattern) =
  { pat with data =
    match pat.data with
    | PWildcard   -> PWildcard
    | PAs(pat, x) -> PAs(rename_pattern ren pat, rename_var ren x)
    | PCtor(name, idx, prf, tvars, pats1, pats2) ->
      PCtor(name, idx, rename_prf_expr ren prf,
             List.map (rename_tvar ren) tvars,
             List.map (rename_pattern ren) pats1,
             List.map (rename_pattern ren) pats2)
    | PAnnot(pat, sch) ->
      PAnnot(rename_pattern ren pat, rename_scheme_expr ren sch)
  }

and rename_prf_poly_expr ren (e : poly_expr) =
  { e with data =
    match e.data with
    | EOptionPrf -> EOptionPrf
    | EVar x     -> EVar(rename_var ren x)
    | EPolyFun(tvs, [], e) ->
      let scope = Scope.enter ren.scope in
      let refresh_named_tvar tvar_map (name, x) =
        let y = TVar.fresh ~scope (TVar.kind x) in
        (TVar.Map.add x y tvar_map, (name, y))
      in
      let (tvar_map, tvs) =
        List.fold_left_map refresh_named_tvar ren.tvar_map tvs in
      let ren = { ren with scope; tvar_map } in
      EPolyFun(tvs, [], rename_prf_expr ren e)

    | EPolyFun(_, _ :: _, _) | EHole _ ->
      (* Theses constructs should not appear in the proofs *)
      assert false
  }

and rename_prf_expr ren (e : expr) =
  { e with data =
    match e.data with
    | EUnitPrf -> EUnitPrf
    | EInst(pe, tps, es) ->
      EInst(rename_prf_poly_expr ren pe,
            List.map (rename_type_expr ren) tps,
            List.map (rename_prf_poly_expr ren) es)

    | ENum _ | ENum64 _ | EStr _ | EChr _ | EFn _ | EAppPoly _ | EAppMono _
    | ELetPoly _ | ELetMono _ | ELetRec _ | ECtor _ | EData _ | EMatchEmpty _
    | EMatch _ | EMatchPoly _ | EHandle _ | EHandler _ | EEffect _ | EExtern _
    | EAnnot _ | ERepl _ | EReplExpr _ ->
      (* These constructs should not appear in the proofs *)
      assert false
  }
