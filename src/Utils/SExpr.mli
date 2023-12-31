(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** S-expressions *)

(* Author: Piotr Polesiuk, 2023 *)

(** Type of S-expressions *)
type t =
  | Sym  of string
  | Num  of int
  | List of t list

(** Pretty-print S-expression to stdout *)
val pretty_stdout : t -> unit

(** Pretty-print S-expression to stdout *)
val pretty_stderr : t -> unit
