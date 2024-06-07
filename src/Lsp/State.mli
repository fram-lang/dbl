(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Server state *)
 
type t
type uri = string

val create : in_channel:in_channel -> out_channel:out_channel -> t

val out_channel : t -> out_channel
val in_channel : t -> in_channel

val open_document : t -> uri -> t

val update_document : t -> uri -> string -> t

val close_document : t -> uri -> t

val close_all_documents : t -> t

val get_document_path : t -> uri -> string

