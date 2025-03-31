(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Generic variable representation.

  Variables are identified by their unique identifier, but they also contain
  name provided by the user. Names are used for pretty-printing and they
  don't need to be unique. *)

type t = private {
  uid  : UID.t; (** Unique identifier *)
  name : string (** Name *)
}

(** Create fresh variable *)
val fresh : ?name:string -> unit -> t

(** Return name of the variable, that is guaranteed to be unique, i.e, for
  different variables, [unique_name] return different names *)
val unique_name : t -> string

(** Equality of variables *)
val equal : t -> t -> bool

(** Finite sets of variables *)
module Set : Set.S with type elt = t

(** Finite maps from variables *)
module Map : Map.S with type key = t
