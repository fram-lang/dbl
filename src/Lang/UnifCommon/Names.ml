(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Names of types and values. *)

(** Name of a named type parameter *)
type tname =
  | TNAnon
    (** Anonymous parameter *)

  | TNVar of string
    (** Regular named type parameter *)

(** Name of a named parameter *)
type name =
  | NVar      of string
    (** Regular named parameter *)

  | NOptionalVar of string
    (** Optional named parameter *)

  | NImplicit of string
    (** Implicit parameter *)

  | NMethod   of string
    (** Name of methods **)
