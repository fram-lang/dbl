(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scopes and operations on them *)

type t

(** Initial empty scope *)
val initial : t

(** Extend the scope with given variable *)
val add : t -> TVar.t -> t

(** Extend the scope with a named type variable *)
val add_named : t -> ('name * TVar.t) -> t

(** Shrink given scope to given level, leaving only those type variables,
  that satisfies given predicate. *)
val filter : int -> (TVar.t -> bool) -> t -> t

(** Check if given type variable is defined in given scope *)
val mem : t -> TVar.t -> bool

(** Permute variables in given scope *)
val perm : TVar.Perm.t -> t -> t

(** Shrink the domain of given partial permutation *)
val shrink_perm_dom : t -> TVar.Perm.t -> TVar.Perm.t

(** Increase a level of given scope *)
val incr_level : t -> t

(** Get a level of given scope *)
val level : t -> int
