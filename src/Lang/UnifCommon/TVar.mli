(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

type t = private {
  uid    : UID.t;
  pp_uid : PPTree.uid;
  kind   : Kind.t;
  scope  : Scope.t
}

(** Kind of a type variable *)
val kind : t -> Kind.t

(** Create fresh type variable of given kind. Optionally, a unique
  identifier used by the pretty-printer can be provided. If omitted, it
  will be the same as the freshly generated unique identifier of the
  variable. *)
val fresh : ?pp_uid:PPTree.uid -> scope:Scope.t -> Kind.t -> t

(** Create a fresh type variable of the same kind and with the same
  pretty-printer UID as the given one. *)
val clone : scope:Scope.t -> t -> t

(** Compare two type variables *)
val compare : t -> t -> int

(** Check type variables for equality *)
val equal : t -> t -> bool

(** Get the unique identifier *)
val uid : t -> UID.t

(** Get the unique identifier for pretty-printing *)
val pp_uid : t -> PPTree.uid

(** Get the scope of a type variable *)
val scope : t -> Scope.t

(** Check if a type variable can be used in a given scope *)
val in_scope : t -> Scope.t -> bool

(** Finite sets of type variables *)
module Set : Set.S with  type elt = t

(** Finite map from type variables *)
module Map : Map.S with  type key = t
