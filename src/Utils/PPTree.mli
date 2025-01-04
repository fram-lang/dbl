(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Trees of modules for pretty-printing of types. *)

type t

(** Empty tree. *)
val empty : t

(** Add the type of given UID to the current module. *)
val add : ?public:bool -> t -> string -> UID.t -> t
