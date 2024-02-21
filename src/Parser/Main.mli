(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the parser *)

(* 2023: Piotr Polesiuk: initial implementation
   2024: Patrycja Balik: add parse_lib *)

(** File name *)
type fname = string

(** Parse single source file *)
val parse_file : ?pos:Position.t -> fname -> Lang.Surface.program

(** Parse a library and prepend it to the program *)
val parse_lib : fname -> Lang.Surface.program -> Lang.Surface.program

(** REPL program *)
val repl : Lang.Surface.program
