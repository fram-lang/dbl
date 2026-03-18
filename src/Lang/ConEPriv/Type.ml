(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Other operations on types and related constructs *)

open TypeBase

let rec collect_gvars ~outer_scope tp gvs =
  match view tp with
  | TVar x -> gvs
  | TArrow(sch, tp, eff) ->
    collect_scheme_gvars ~outer_scope sch gvs
    |> collect_gvars ~outer_scope tp
    |> CEffect.collect_gvars ~outer_scope eff
  | TLabel(eff, delim_tp, delim_eff) ->
    Effct.collect_gvars ~outer_scope eff gvs
    |> collect_gvars ~outer_scope delim_tp
    |> Effct.collect_gvars ~outer_scope delim_eff
  | THandler { tvar = _; cap_tp; in_tp; in_eff; out_tp; out_eff } ->
    collect_gvars ~outer_scope cap_tp gvs
    |> collect_gvars ~outer_scope in_tp
    |> Effct.collect_gvars ~outer_scope in_eff
    |> collect_gvars ~outer_scope out_tp
    |> Effct.collect_gvars ~outer_scope out_eff
  | TEffect eff ->
    Effct.collect_gvars ~outer_scope eff gvs
  | TApp(tp1, tp2) ->
    collect_gvars ~outer_scope tp1 gvs |> collect_gvars ~outer_scope tp2
  | TAlias(_, tp) ->
    collect_gvars ~outer_scope tp gvs

and collect_scheme_gvars ~outer_scope sch gvs =
  List.fold_left
    (fun gvs (_, sch) -> collect_scheme_gvars ~outer_scope sch gvs)
    gvs
    sch.sch_named
  |> collect_gvars ~outer_scope sch.sch_body

(* ========================================================================= *)

let swap_pair (x, y) = (y, x)

let collect_effect_gvars_p ~outer_scope eff (pgvs, ngvs) =
  (Effct.collect_gvars ~outer_scope eff pgvs, ngvs)

let collect_effect_gvars_n ~outer_scope eff (pgvs, ngvs) =
  (pgvs, Effct.collect_gvars ~outer_scope eff ngvs)

let collect_ceffect_gvars_p ~outer_scope eff (pgvs, ngvs) =
  (CEffect.collect_gvars ~outer_scope eff pgvs, ngvs)

let collect_gvars_i ~outer_scope tp (pgvs, ngvs) =
  let gvs = collect_gvars ~outer_scope tp Effct.GVar.Set.empty in
  (Effct.GVar.Set.union gvs pgvs, Effct.GVar.Set.union gvs ngvs)

let rec collect_gvars_p ~outer_scope tp gvsp =
  match view tp with
  | TVar x -> gvsp
  | TArrow(sch, tp, eff) ->
    collect_scheme_gvars_n ~outer_scope sch gvsp
    |> collect_gvars_p ~outer_scope tp
    |> collect_ceffect_gvars_p ~outer_scope eff
  | THandler { tvar = _; cap_tp; in_tp; in_eff; out_tp; out_eff } ->
    collect_gvars_p ~outer_scope cap_tp gvsp
    |> collect_gvars_n ~outer_scope in_tp
    |> collect_effect_gvars_n ~outer_scope in_eff
    |> collect_gvars_p ~outer_scope out_tp
    |> collect_effect_gvars_p ~outer_scope out_eff
  | TLabel _ | TApp _ ->
    collect_gvars_i ~outer_scope tp gvsp
  | TAlias(_, tp) ->
    collect_gvars_p ~outer_scope tp gvsp
  
  | TEffect _ -> failwith "Internal kind error"

and collect_gvars_n ~outer_scope tp gvsp =
  gvsp |> swap_pair |> collect_gvars_p ~outer_scope tp |> swap_pair

and collect_scheme_gvars_p ~outer_scope sch gvsp =
  List.fold_left
    (fun gvsp (_, sch) -> collect_scheme_gvars_n ~outer_scope sch gvsp)
    gvsp
    sch.sch_named
  |> collect_gvars_p ~outer_scope sch.sch_body

and collect_scheme_gvars_n ~outer_scope sch gvsp =
  gvsp |> swap_pair |> collect_scheme_gvars_p ~outer_scope sch |> swap_pair

(* ========================================================================= *)

let scheme_to_type sch =
  match sch with
  | { sch_targs = []; sch_named = []; sch_body = tp } -> Some tp
  | _ -> None

let is_monomorphic sch =
  match scheme_to_type sch with
  | Some _ -> true
  | None -> false

let scheme_of_type tp =
  { sch_targs = [];
    sch_named = [];
    sch_body  = tp
  }
