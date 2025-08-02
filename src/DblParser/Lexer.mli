(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Lexer *)

val reset : unit -> unit

val token : Lexing.lexbuf -> YaccParser.token
