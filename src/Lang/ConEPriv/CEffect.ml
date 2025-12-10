(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Computation effects with distinguished purity *)

type t =
  | Pure
  | Impure of Effct.t

let prog_effect = Impure (Effct.var UnifCommon.BuiltinType.tv_io)

let join eff1 eff2 =
  match eff1, eff2 with
  | Pure, _ -> eff2
  | _, Pure -> eff1
  | Impure eff1, Impure eff2 -> Impure (Effct.join eff1 eff2)

let collect_gvars ~outer_scope eff gvs =
  match eff with
  | Pure       -> gvs
  | Impure eff -> Effct.collect_gvars ~outer_scope eff gvs

let to_sexpr eff =
  match eff with
  | Pure       -> SExpr.Sym "pure"
  | Impure eff -> Effct.to_sexpr eff
