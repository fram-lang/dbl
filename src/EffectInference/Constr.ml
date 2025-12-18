(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Effect constraints *)

open Common

type t =
  | CSubeffect of origin * T.effct * T.effct

let to_sexpr (CSubeffect(_, eff1, eff2)) =
  SExpr.List [T.Effct.to_sexpr eff1; Sym "<:"; T.Effct.to_sexpr eff2]

let collect_constr_gvars ~outer_scope c gvs =
  match c with
  | CSubeffect(_, eff1, eff2) ->
    gvs
    |> T.Effct.collect_gvars ~outer_scope eff1
    |> T.Effct.collect_gvars ~outer_scope eff2

let collect_gvars ~outer_scope cs gvs =
  List.fold_left (fun gvs c -> collect_constr_gvars ~outer_scope c gvs) gvs cs
