(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on proof expressions. *)

open Syntax

val subst : Subst.t -> proof_expr -> proof_expr
