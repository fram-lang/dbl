(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pipeline of the compiler *)

(* Author: Piotr Polesiuk, 2023 *)

(** Dump internal Core representation if this flag is set. *)
val dump_core : bool ref

(** Run in REPL mode *)
val run_repl : unit -> unit

(** Run single file as a program. It takes file path as a parameter. *)
val run_file : string -> unit
