(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module for reporting errors *)

(* Author: Piotr Polesiuk, 2023 *)

(** Fatal error, that aborts compilation *)
exception Fatal_error

(** Increase error counter *)
val incr_error_counter : unit -> unit

(** Abort compilation if any error was reported. *)
val assert_no_error : unit -> unit

(** Reset state of reported errors. Used in REPL in case of an error. *)
val reset : unit -> unit
