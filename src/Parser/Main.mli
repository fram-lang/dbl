(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the parser *)

(* Author: Piotr Polesiuk, 2023 *)

(** File name *)
type fname = string

(** Parse single source file *)
val parse_file : ?pos:Position.t -> fname -> Lang.Surface.program
