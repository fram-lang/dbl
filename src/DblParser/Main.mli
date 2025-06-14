(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the parser *)

(** File name *)
type fname = string

(** Parse single source file *)
val parse_file :
  ?pos:Position.t -> use_prelude:bool -> fname -> Lang.Surface.program

(** REPL program *)
val repl : use_prelude:bool -> use_toString:bool -> Lang.Surface.program
