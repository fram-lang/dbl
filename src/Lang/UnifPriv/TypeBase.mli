(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

(* Author: Piotr Polesiuk, 2023 *)

open KindBase

type uvar
type tvar = TVar.t
type typ

type effect = typ

type type_view =
  | TUnit
  | TRowPure
  | TUVar      of uvar
  | TVar       of tvar
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

(** Pure effect row *)
val t_row_pure : effect

(** Reveal a top-most constructor of a type *)
val view : typ -> type_view

(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  val fresh : kind -> uvar

  val equal : t -> t -> bool

  val set : t -> typ -> unit

  val fix : t -> tvar

  module Set : Set.S with type elt = t
end
