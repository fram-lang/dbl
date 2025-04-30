(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of effects in the ConE language. *)

(** Formulas used for effect guards. *)
type formula = IncrSAT.Formula.t

(** Generalizable variable *)
type gvar

type t

(** Operations on generalizable variables *)
module GVar : sig
  (** Get the scope of a generalizable variable *)
  val scope : gvar -> Scope.t

  (** Check if generalizable variable belongs to the given scope. *)
  val in_scope : gvar -> Scope.t -> bool

  (** Get the unique identifier *)
  val uid : gvar -> UID.t

  (** Update the scope of generalizable variable. See [Lang.ConE] for details.
    *)
  val update_scope : scope:Scope.t -> tvars:TVar.t list -> gvar -> gvar

  (** Globally substitute an effect for a given generalizable variable.
    The effect should belongs to the scope of the variable. This operation
    can be done only once for each generalizable variable. *)
  val set : gvar -> t -> unit

  (** Promote to regular type variable. *)
  val fix : gvar -> TVar.t

  (** Pretty-print the generalizable variable as S-expression. *)
  val to_sexpr : gvar -> SExpr.t

  (** Finite sets of generalizable variables *)
  module Set : Set.S with type elt = gvar

  (** Finite maps from generalizable variables *)
  module Map : Map.S with type key = gvar
end

(** Pure effect. It still allows non-termination. *)
val pure : t

(** Effect that contains a single effect variable. *)
val var : TVar.t -> t

(** Effect that contains a single generalizable variable *)
val gvar : gvar -> t

(** Extend the effect with a type variable *)
val cons : TVar.t -> formula -> t -> t

(** Join of two effects *)
val join : t -> t -> t

(** Create effect that is equal to the given effect, when the formula is
  satisfied, and equal to the pure effect when the formula is not satisfied.
  *)
val guard : t -> formula -> t

(** Remove all components of the effect that do not belong to the given
  scope. *)
val filter_to_scope : scope:Scope.t -> t -> t

(** Generate a fresh generalizable variable and convert it into an effect. *)
val fresh_gvar : scope:Scope.t -> t

(** Reveal the representation of the effect: two list of atomic effects
  with predicates that states if the atomic effect belongs to the effect. *)
val view : t -> (TVar.t * formula) list * (gvar * formula) list

(** Split the effect into a list of type variables (together with the
  predicates), and the remaining effect built only from generalizable
  variables. *)
val take_tvars : t -> (TVar.t * formula) list * t

(** Lookup a type variable in the effect. Returns formula that indicates if
  a type variable is included in the effect. *)
val lookup_tvar : t -> TVar.t -> formula

(** Lookup a generalizable variable in the effect. Returns formula that
  indicates if a generalizable variable is included in the effect. *)
val lookup_gvar : t -> gvar -> formula

(** Collect all generalizable variables that do not belong to the given
  scope and add them to the given set. *)
val collect_gvars : scope:Scope.t -> t -> GVar.Set.t -> GVar.Set.t

(** Pretty-print the effect as S-expression. *)
val to_sexpr : t -> SExpr.t
