(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scope of type variables.

  We could think of a scope as a set of type variables. However, they are
  represented as paths to the root in a tree, where each node represents a
  place where a type variable can be bound. *)

type t

(** Root scope. It cannot contain any type variable. *)
val root : t

(** Initial scope. Builtin types are defined at this scope. *)
val initial : t

(** A scope that is not present in the tree rooted at the [root] scope.
  The [parent], [enter], and [inter] functions should not be called on this
  scope. *)
val any : t

(** Enter a new scope. The provided scope becomes the parent of the new one.
  *)
val enter : t -> t

(** Get the parent of a scope. Root scope has no parent. *)
val parent : t -> t

(** Intersection of two scopes. *)
val inter : t -> t -> t

(** Check if two scopes are equal. *)
val equal : t -> t -> bool

(** Check if the first scope is a subset of the second one. In other words,
  check if the first scope is a path starting at node that is an ancestor of
  the second scope. *)
val subset : t -> t -> bool

(** Check if the first scope is a strict subset of the second one. Being a
  strict subset doesn't mean that the scopes treated as sets are different:
  the second scope may contain additional places of binding which don't
  actually bind any type variable. *)
val strict_subset : t -> t -> bool

(** Check if a type variable marked as belonging to the first scope can be
  used in the second scope. *)
val mem : t -> t -> bool

(** Pretty-print a scope as an S-expression. *)
val to_sexpr : t -> SExpr.t
