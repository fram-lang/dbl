(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Effect constraints *)

open Common

(** Constraints *)
type t =
  | CSubeffect of origin * T.effct * T.effct
    (** Subeffect constraint. *)

(** Collect all generalizable variables that do not belong to the given
  scope and add them to the given set. *)
val collect_gvars : scope:Scope.t -> t list -> T.GVar.Set.t -> T.GVar.Set.t

(** Pretty-print constraint as S-expression *)
val to_sexpr : t -> SExpr.t
