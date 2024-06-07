(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Base HTML-like protocol *)

exception Connection_error of string

val receive_string : in_channel -> string

val send_string : out_channel -> string -> unit

