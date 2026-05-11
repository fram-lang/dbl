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

let compare_tname t1 t2 =
  match t1, t2 with
  | TNAnon, TNAnon -> 0
  | TNAnon, _ -> -1
  | _, TNAnon -> 1
  | TNVar n1, TNVar n2 -> String.compare n1 n2

let compare name1 name2 =
  match name1, name2 with
  | NVar n1, NVar n2 -> String.compare n1 n2
  | NVar _, _ -> -1
  | _, NVar _ -> 1
  | NOptionalVar n1, NOptionalVar n2 -> String.compare n1 n2
  | NOptionalVar _, _ -> -1
  | _, NOptionalVar _ -> 1
  | NImplicit n1, NImplicit n2 -> String.compare n1 n2
  | NImplicit _, _ -> -1
  | _, NImplicit _ -> 1
  | NMethod n1, NMethod n2 -> String.compare n1 n2

let equal n1 n2 = compare n1 n2 = 0
