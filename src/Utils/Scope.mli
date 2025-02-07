(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scope of type variables.

  We could think of a scope as a set of type variables. However, they are
  represented as paths to the root in a tree, where each node represents a
  place where a type variable can be bound. *)

type t

(** Initial scope. *)
val root : t

(** Enter a new scope. The provided scope becomes the parent of the new one.
  *)
val enter : t -> t

(** Intersection of two scopes. *)
val inter : t -> t -> t

(** Check if the first scope is a subset of the second one. In other words, if
  a type variable marked as belonging to the first scope can be used in the
  second scope. *)
val mem : t -> t -> bool
