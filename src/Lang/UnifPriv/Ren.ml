(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Variable renaming. *)

open UnifCommon
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

let rename_proof_expr ren (e : proof_expr) =
  match e with
  | PE_Unit        -> PE_Unit
  | PE_Bool        -> PE_Bool
  | PE_Option tp   -> PE_Option(rename_type ren tp)
  | PE_Var(x, tps) -> PE_Var(rename_var ren x, List.map (rename_type ren) tps)

let rec rename_pattern ren (pat : pattern) =
  { pat with data =
    match pat.data with
    | PWildcard   -> PWildcard
    | PAs(pat, x) -> PAs(rename_pattern ren pat, rename_var ren x)
    | PCtor(name, idx, prf, tvars, pats1, pats2) ->
      PCtor(name, idx, rename_proof_expr ren prf,
             List.map (rename_tvar ren) tvars,
             List.map (rename_pattern ren) pats1,
             List.map (rename_pattern ren) pats2)
    | PAnnot(pat, sch) ->
      PAnnot(rename_pattern ren pat, rename_scheme_expr ren sch)
  }
