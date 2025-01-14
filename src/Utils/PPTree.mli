(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Trees of modules for pretty-printing of types. *)

type t

(** Representation of a module containing types and other modules. *)
type pp_module

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
val add : public:bool -> ?pos:Position.t -> t -> string -> UID.t -> t

(** Add anonymous type to the current module. *)
val add_anon : ?pos:Position.t -> ?name:string -> t -> UID.t -> t

(** Create a new module on top of the module stack. *)
val enter_module : t -> t

(** Finalize a module definition and add it to the outer module with the
  given name. *)
val leave_module : public:bool -> t -> string -> t * pp_module

(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : public:bool -> t -> pp_module -> t

(** Lookup for a unique name of the type of given UID. *)
val lookup : t -> UID.t -> pp_result
