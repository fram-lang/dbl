(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

type t = UnifCommon.TVar.t

(** Kind of a type variable *)
val kind : t -> UnifCommon.Kind.t

(** Create fresh type variable of the effect kind. *)
val fresh_eff : scope:Scope.t -> t

(** Fresh type variable, that uses metadata (ppuid, kind) from the given
  Unif type variable *)
val clone_unif : scope:Scope.t -> UnifCommon.TVar.t -> t

(** Fresh type variable that uses metadata from the given type variable *)
val clone : scope:Scope.t -> t -> t

(** Check equality of type variables *)
val equal : t -> t -> bool

(** Check if a type variable can be used in a given scope *)
val in_scope : t -> Scope.t -> bool

(** Finite sets of type variables *)
module Set : Set.S with type elt = t

(** Finite map from type variables *)
module Map : Map.S with type key = t

(** Pretty-print type variable as S-expression *)
val to_sexpr : t -> SExpr.t
