(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of names. It differs from the names in Unif in that
  the method owner is explicit. *)

open Common

(** Method owner. *)
type method_owner =
  | MO_Arrow
    (** Arrow type. *)

  | MO_Handler
    (** Handler type. *)

  | MO_Label
    (** Label type. *)

  | MO_TVar of T.tvar
    (** Abstract type or ADT. *)

(** Name. *)
type t =
  | NVar         of string
  | NOptionalVar of string
  | NImplicit    of string
  | NMethod      of method_owner * string

(** Type schemes that use more precise names *)
type scheme =
  { sch_targs : T.named_tvar list;
    sch_named : (t * T.scheme) list;
    sch_body  : T.typ
  }

(** Named pattern that uses more precise names *)
type pattern = t * T.pattern * T.scheme_expr

(** Translate a name to Unif representation. *)
val to_unif : t -> T.name

(** Get the string used in given name. Not necessarily unique. *)
val to_string : t -> string

(** Name comparison. Regular names and optional names are considered equal. *)
val compare : t -> t -> int

(** Name equality. Regular names and optional names are considered equal. *)
val equal : t -> t -> bool

(** Get a set of unification variables in given scheme. *)
val scheme_uvars : scheme -> T.UVar.Set.t

(** Create a monomorphic scheme. *)
val scheme_of_type : T.typ -> scheme

(** Finite sets of names. *)
module Set : Set.S with type elt = t
(** Finite maps from names. *)
module Map : Map.S with type key = t
