(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Trees of modules for pretty-printing of types. *)

type t

(** Result of a lookup. *)
type pp_result =
  | Found   of string
    (** The type was found and is representable *)

  | Anon    of string * Position.t option
    (** The type was found but is not representable. The optional position
      is the one where the type was defined. *)

  | Unbound of string
    (** The type was not found. *)

(** Empty tree. *)
val empty : t

(** Add the type of given UID to the current module. *)
val add : ?public:bool -> ?pos:Position.t -> t -> string -> UID.t -> t

(** Lookup for a unique name of the type of given UID. *)
val lookup : t -> UID.t -> pp_result
