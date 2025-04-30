(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Other operations on types and related constructs *)

open TypeBase

let rec collect_gvars ~scope tp gvs =
  match view tp with
  | TVar x -> gvs
  | TArrow(sch, tp, eff) ->
    collect_scheme_gvars ~scope sch gvs
    |> collect_gvars ~scope tp
    |> CEffect.collect_gvars ~scope eff
  | TLabel(eff, delim_tp, delim_eff) ->
    Effct.collect_gvars ~scope eff gvs
    |> collect_gvars ~scope delim_tp
    |> Effct.collect_gvars ~scope delim_eff
  | THandler { tvar = _; cap_tp; in_tp; in_eff; out_tp; out_eff } ->
    collect_gvars ~scope cap_tp gvs
    |> collect_gvars ~scope in_tp
    |> Effct.collect_gvars ~scope in_eff
    |> collect_gvars ~scope out_tp
    |> Effct.collect_gvars ~scope out_eff
  | TEffect eff ->
    Effct.collect_gvars ~scope eff gvs
  | TApp(tp1, tp2) ->
    collect_gvars ~scope tp1 gvs |> collect_gvars ~scope tp2

and collect_scheme_gvars ~scope sch gvs =
  List.fold_left
    (fun gvs (_, sch) -> collect_scheme_gvars ~scope sch gvs)
    gvs
    sch.sch_named
  |> collect_gvars ~scope sch.sch_body

(* ========================================================================= *)

let swap_pair (x, y) = (y, x)

let collect_effect_gvars_p ~scope eff (pgvs, ngvs) =
  (Effct.collect_gvars ~scope eff pgvs, ngvs)

let collect_effect_gvars_n ~scope eff (pgvs, ngvs) =
  (pgvs, Effct.collect_gvars ~scope eff ngvs)

let collect_ceffect_gvars_p ~scope eff (pgvs, ngvs) =
  (CEffect.collect_gvars ~scope eff pgvs, ngvs)

let collect_gvars_i ~scope tp (pgvs, ngvs) =
  let gvs = collect_gvars ~scope tp Effct.GVar.Set.empty in
  (Effct.GVar.Set.union gvs pgvs, Effct.GVar.Set.union gvs ngvs)

let rec collect_gvars_p ~scope tp gvsp =
  match view tp with
  | TVar x -> gvsp
  | TArrow(sch, tp, eff) ->
    collect_scheme_gvars_n ~scope sch gvsp
    |> collect_gvars_p ~scope tp
    |> collect_ceffect_gvars_p ~scope eff
  | THandler { tvar = _; cap_tp; in_tp; in_eff; out_tp; out_eff } ->
    collect_gvars_p ~scope cap_tp gvsp
    |> collect_gvars_n ~scope in_tp
    |> collect_effect_gvars_n ~scope in_eff
    |> collect_gvars_p ~scope out_tp
    |> collect_effect_gvars_p ~scope out_eff
  | TLabel _ | TApp _ ->
    collect_gvars_i ~scope tp gvsp
  
  | TEffect _ -> failwith "Internal kind error"

and collect_gvars_n ~scope tp gvsp =
  gvsp |> swap_pair |> collect_gvars_p ~scope tp |> swap_pair

and collect_scheme_gvars_p ~scope sch gvsp =
  List.fold_left
    (fun gvsp (_, sch) -> collect_scheme_gvars_n ~scope sch gvsp)
    gvsp
    sch.sch_named
  |> collect_gvars_p ~scope sch.sch_body

and collect_scheme_gvars_n ~scope sch gvsp =
  gvsp |> swap_pair |> collect_scheme_gvars_p ~scope sch |> swap_pair

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
