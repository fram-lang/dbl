(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Lexer *)

(* Author: Piotr Polesiuk, 2023 *)

val token : Lexing.lexbuf -> YaccParser.token
