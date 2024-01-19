(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to parsing *)

(* Author: Piotr Polesiuk, 2023,2024 *)

(** Abstract representation of error *)
type t

(** Raise fatal error and abort the compilation *)
val fatal : t -> 'a

(** Report a warning. *)
val warn : t -> unit

val cannot_read_file : ?pos:Position.t -> fname:string -> string -> t
val cannot_open_file : ?pos:Position.t -> fname:string -> string -> t

val unexpected_token  : Position.t -> string -> t
val invalid_character : Position.t -> char -> t

val eof_in_comment : Position.t -> t

val desugar_error : Position.t -> t
val invalid_pattern_arg : Position.t -> t
val impure_scheme : Position.t -> t
val anon_type_pattern : Position.t -> t

val value_before_type_param : Position.t -> t
