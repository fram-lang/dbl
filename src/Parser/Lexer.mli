(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Lexer *)

val token : Lexing.lexbuf -> YaccParser.token
