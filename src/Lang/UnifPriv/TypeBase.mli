(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open KindBase

type name = string

type uvar
type tvar  = TVar.t
type scope = TVar.Set.t
type typ

type effect = typ

type effect_end =
  | EEClosed
  | EEUVar of TVar.Perm.t * uvar
  | EEVar  of tvar
  | EEApp  of typ * typ

type type_view =
  | TUnit
  | TUVar      of TVar.Perm.t * uvar
  | TVar       of tvar
  | TEffect    of TVar.Set.t * effect_end
  | TPureArrow of scheme * typ
  | TArrow     of scheme * typ * effect
  | TApp       of typ * typ

and scheme = {
  sch_tvars    : tvar list;
  sch_implicit : (name * scheme) list;
  sch_body     : typ
}

type ctor_decl = {
  ctor_name        : string;
  ctor_tvars       : tvar list;
  ctor_implicit    : (name * scheme) list;
  ctor_arg_schemes : scheme list
}

(** Unit type *)
val t_unit : typ

(** Unification variable *)
val t_uvar : TVar.Perm.t -> uvar -> typ

(** Regular type variable *)
val t_var : tvar -> typ

(** Pure arrow type *)
val t_pure_arrow : scheme -> typ -> typ

(** Arrow type *)
val t_arrow : scheme -> typ -> effect -> typ

(** Create an effect *)
val t_effect : TVar.Set.t -> effect_end -> effect

(** Create a closed effect *)
val t_closed_effect : TVar.Set.t -> effect

(** Type application *)
val t_app : typ -> typ -> typ

(** Reveal a top-most constructor of a type *)
val view : typ -> type_view

(** Reveal a representation of an effect: a set of effect variables together
  with a way of closing an effect. *)
val effect_view : effect -> TVar.Set.t * effect_end

(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  val fresh : scope:scope -> kind -> uvar

  (** Get the kind of given unification variable *)
  val kind : t -> kind

  val equal : t -> t -> bool

  (** Set a unification variable, without checking any constraints. It returns
    expected scope of set type. The first parameter is a permutation attached
    to unification variable *)
  val raw_set : TVar.Perm.t -> t -> typ -> scope

  val fix : t -> tvar

  (** Shrink scope of given unification variable to intersection of current
    and given scope. *)
  val shrink_scope : scope:scope -> uvar -> unit

  (** Shrink scope of given unification variable, leaving only those variables
    which satisfy given predicate *)
  val filter_scope : uvar -> (tvar -> bool) -> unit

  module Set : Set.S with type elt = t
end
