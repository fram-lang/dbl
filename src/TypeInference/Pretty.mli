(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pretty printing of types and kinds *)

open Common

(** Contexts of pretty-printing *)
type ctx

(** Create an empty context. *)
val empty_context : unit -> ctx

(** Pretty-print kind *)
val kind_to_string : ctx -> T.kind -> string
(*
(** Pretty-print type name *)
val tname_to_string : T.tname -> string
*)
(** Pretty-print type variable *)
val tvar_to_string : ctx -> Env.t -> T.tvar -> string

(** Pretty-print type *)
val type_to_string : ctx -> Env.t -> T.typ -> string
(*
(** Pretty-print type scheme *)
val scheme_to_string : ctx -> Env.t -> T.scheme -> string
*)
(** Pretty-print additional information about printing context, e.g.,
  locations of binders of anonymous types. *)
val additional_info : ctx -> string
