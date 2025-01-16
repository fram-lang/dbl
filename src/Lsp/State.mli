(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Server state *)

(** The LSP client claims ownership of files edited by the user (by sending
  a `textDocument/didOpen` notification). That means the content of the file
  is managed by the client and shouldn't be read from disk because the user
  might not have saved it yet. The client informs us of the changes in the file
  with a `textDocument/didChange` notification. We keep our own copy of
  the file in a temporary location and update it based on those notifications.
  The file is closed with a `textDocument/didClose` notification.

  Note from the spec: a server’s ability to fulfill requests is independent
    of whether a text document is open or closed.
  Spec: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
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

(** Close all documents.
  This removes all of the temporary files. *)
val close_all_documents : t -> t

(** Get a path to the file associated with the specified uri.
  If the client has opened the file, this returns the path to the temp file;
  otherwise, this returns the same path that the uri points to. *)
val get_document_path : t -> Uri.t -> string
