(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint simplification before generalization *)

open Common

(** Simplify the set of constraints. The [outer_scope] is an outer scope of
  the place of the generalization. Variables in [pgvs] are those that appear
  on non-negative positions, and therefore cannot be promoted to supereffects.
  Dually, [ngvs] appears on non-positive positions, so cannot be downgraded
  to subeffects. *)
val simplify :
  outer_scope:Scope.t -> pgvs:T.GVar.Set.t -> ngvs:T.GVar.Set.t ->
    Constr.t list -> Constr.t list
