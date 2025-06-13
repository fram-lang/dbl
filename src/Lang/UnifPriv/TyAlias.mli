(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type aliases *)

type t

(** Create fresh type alias. Optionally, a unique identifier used by the
  pretty-printer can be provided. If omitted, it will be the same as the
  freshly generated unique identifier of the alias. *)
val fresh : ?pp_uid:PPTree.uid -> scope:Scope.t -> unit -> t

(** Check type aliases for equality *)
val equal : t -> t -> bool

(** Get the unique identifier for pretty-printing *)
val pp_uid : t -> PPTree.uid

(** Check if an alias can be used in a given scope *)
val in_scope : t -> Scope.t -> bool

(** Finite map from type aliases *)
module Map : Map.S with type key = t
