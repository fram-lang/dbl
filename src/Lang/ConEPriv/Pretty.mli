(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pretty-printing of types *)

open TypeBase

(** Context of pretty-printing *)
type ctx

(** Create an empty context. *)
val empty_context : unit -> ctx

(** Pretty-print type variable *)
val pp_tvar : ctx -> PPTree.t -> tvar -> string

(** Pretty-print the effect *)
val pp_effect : ctx -> PPTree.t -> effct -> string

(** Pretty-print type *)
val pp_type : ctx -> PPTree.t -> typ -> string

(** Pretty-print type scheme *)
val pp_scheme : ctx -> PPTree.t -> scheme -> string

(** Pretty-print additional information about printing context, e.g.,
  locations of binders of anonymous types. *)
val additional_info : ctx -> string
