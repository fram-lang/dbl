(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

(* Author: Piotr Polesiuk, 2023 *)

open KindBase

type uvar
type tvar = TVar.t
type typ

type type_view =
  | TUnit
  | TUVar  of uvar
  | TVar   of tvar
  | TArrow of typ * typ

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

(** Arrow type *)
val t_arrow : typ -> typ -> typ

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
