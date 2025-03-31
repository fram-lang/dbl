(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Computation effects with distinguished purity *)

type t =
  | Pure
  | Impure of Effct.t

(** Expected effect of the whole program *)
val prog_effect : t

(** Join two computation effects. *)
val join : t -> t -> t

(** Collect all generalizable variables that do not belong to the given
  scope and add them to the given set. *)
val collect_gvars : scope:Scope.t -> t -> Effct.GVar.Set.t -> Effct.GVar.Set.t

(** Pretty-print effect as S-expression *)
val to_sexpr : t -> SExpr.t
