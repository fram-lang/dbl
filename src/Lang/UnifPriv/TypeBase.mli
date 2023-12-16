(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

(* Author: Piotr Polesiuk, 2023 *)

open KindBase

type uvar
type tvar  = TVar.t
type scope = TVar.Set.t
type typ

type effect = typ

type effect_end =
  | EEClosed
  | EEVar  of tvar
  | EEUVar of uvar

type type_view =
  | TUnit
  | TUVar      of uvar
  | TVar       of tvar
  | TEffect    of TVar.Set.t * effect_end
  | TPureArrow of typ * typ
  | TArrow     of typ * typ * effect

type scheme = {
  sch_tvars : tvar list;
  sch_body  : typ
}

(** Unit type *)
val t_unit : typ

(** Unification variable *)
val t_uvar : uvar -> typ

(** Regular type variable *)
val t_var : tvar -> typ

(** Pure arrow type *)
val t_pure_arrow : typ -> typ -> typ

(** Arrow type *)
val t_arrow : typ -> typ -> effect -> typ

(** Create an effect *)
val t_effect : TVar.Set.t -> effect_end -> effect

(** Create a closed effect *)
val t_closed_effect : TVar.Set.t -> effect

(** Reveal a top-most constructor of a type *)
val view : typ -> type_view

(** Reveal a representation of an effect: a set of effect variables together
  with a way of closing an effect. *)
val effect_view : effect -> TVar.Set.t * effect_end

(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  val fresh : scope:scope -> kind -> uvar

  val equal : t -> t -> bool

  (** Set a unification variable, without checking any constraints. It returns
    expected scope of set type *)
  val raw_set : t -> typ -> scope

  val fix : t -> tvar

  (** Shrink scope of given unification variable to intersection of current
    and given scope. *)
  val shrink_scope : scope:scope -> uvar -> unit

  module Set : Set.S with type elt = t
end
