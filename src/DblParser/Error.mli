(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to parsing *)

(** Abstract representation of error *)
type t

(** Raise fatal error and abort the compilation *)
val fatal : t -> 'a

(** Report a warning. *)
val warn : t -> unit

val cannot_read_file : ?pos:Position.t -> fname:string -> string -> t
val cannot_open_file : ?pos:Position.t -> fname:string -> string -> t

val cannot_find_module : Position.t -> string -> t
val module_dependency_cycle : string -> t

val unexpected_token  : Position.t -> string -> t
val invalid_character : Position.t -> char -> t

val eof_in_comment : Position.t -> t

val invalid_number : Position.t -> string -> t
val number_out_of_bounds : Position.t -> string -> t

val invalid_escape_code : Position.t -> t
val eof_in_string       : Position.t -> t

val desugar_error : Position.t -> t
val reserved_binop_error : Position.t -> string -> t
val invalid_pattern_arg : Position.t -> t
val impure_scheme : Position.t -> t
val anon_type_pattern : Position.t -> t

val value_before_type_param : Position.t -> t
val finally_before_return_clause : Position.t -> t

val multiple_self_parameters : Position.t -> t
