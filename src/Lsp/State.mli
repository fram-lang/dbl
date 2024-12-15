(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Server state *)

(** The LSP client sends a notification when the user opens, updates
  and closes a file. The server keeps the latest version of each file
  in a temporary location and performs type checks on those.
*)

type t

(** Create a new state *)
val create : in_channel:in_channel -> out_channel:out_channel -> t

(** Access the channels *)
val out_channel : t -> out_channel
val in_channel  : t -> in_channel

(** Open a new document with the given content.
  This creates a new temporary file associated with the specified uri. *)
val open_document : t -> Uri.t -> string -> t

(** Update an open document.
  This updates the temporary file. *)
val update_document : t -> Uri.t -> string -> t

(** Close a document.
  This removes the temporary file. *)
val close_document : t -> Uri.t -> t

(** Close all documents *)
val close_all_documents : t -> t

(** Get a path to the temporary file associated with the specified uri *)
val get_document_path : t -> Uri.t -> string
