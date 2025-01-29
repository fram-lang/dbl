(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Base HTTP-like protocol *)

(** An exception indicating a problem with the connection,
  e.g. unexpected EOF or invalid headers *)
exception Connection_error of string

(** Receive a message from the client *)
val receive_string : in_channel -> string

(** Send a message with the specified content to the client *)
val send_string : out_channel -> string -> unit
