(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

type t = private {
  uid       : UID.t;
  method_ns : UID.t;
  pp_uid    : PPTree.uid;
  kind      : Kind.t;
  scope     : Scope.t
}

(** Kind of a type variable *)
val kind : t -> Kind.t

(** Create a fresh type variable of given kind. There are two unique
  identifiers that can be optionally provided (if omitted, they will be the
  same as the freshly generated unique identifier of the variable):
  - [method_ns] will be used to determine the method namespace associated with
    the type variable;
  - [pp_uid] will be used by the pretty-printer to identify the type variable.
*)
val fresh :
  ?method_ns:UID.t -> ?pp_uid:PPTree.uid -> scope:Scope.t -> Kind.t -> t

(** Create a fresh type variable of the same kind and with the same
  pretty-printer UID as the given one. *)
val clone : scope:Scope.t -> t -> t

(** Compare two type variables *)
val compare : t -> t -> int

(** Check type variables for equality *)
val equal : t -> t -> bool

(** Get the unique identifier *)
val uid : t -> UID.t

(** Get the method namespace identifier *)
val method_ns : t -> UID.t

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

(** Pretty-print type variable as S-expression *)
val to_sexpr : t -> SExpr.t
