(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Generic variable representation.

  Variables are idetified by their unique idetifier, but they also contain
  name provided by the user. Names are used for pretty-printing and they
  don't need to be unique. *)

(* Author: Piotr Polesiuk, 2023 *)

type t = private {
  uid  : UID.t; (** Unique identifier *)
  name : string (** Name *)
}

(** Create fresh variable *)
val fresh : ?name:string -> unit -> t

(** Finite maps from variables *)
module Map : Map.S with type key = t
