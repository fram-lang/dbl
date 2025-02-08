(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Variable renaming. *)

open TypeBase
open Syntax

type t

(** Empty renaming *)
val empty : scope:Scope.t -> t

(** Extend renaming with a renaming of a type variable *)
val add_tvar : t -> tvar -> tvar -> t

(** Extend renaming with a renaming of a regular variable *)
val add_var : t -> var -> var -> t

(** Rename type variable binder *)
val rename_tvar : t -> tvar -> tvar

(** Rename named type variable binder *)
val rename_named_tvar : t -> named_tvar -> named_tvar

(** Rename named type variable binders *)
val rename_named_tvars : t -> named_tvar list -> named_tvar list

(** Rename type *)
val rename_type : t -> typ -> typ

(** Rename type scheme *)
val rename_scheme : t -> scheme -> scheme

(** Rename type scheme expression *)
val rename_scheme_expr : t -> scheme_expr -> scheme_expr

(** Rename variables in pattern *)
val rename_pattern : t -> pattern -> pattern
