(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on internal representation of names. *)

open Common

(** Translate a self type to [method_owner]. [None] means that the type
  is an unification variable, potentially applied to some type arguments. *)
val method_owner_of_self : T.typ -> Name.method_owner option

(** Extract the method owner (self) of given method scheme *)
val method_owner_of_scheme :
  pos:Position.t -> pp:PPTree.t -> T.scheme -> Name.method_owner

(** Translate a name to an internal representation. *)
val tr_name : pos:Position.t -> pp:PPTree.t -> T.name -> T.scheme -> Name.t

(** Translate an identifier to an internal representation. *)
val tr_ident : pos:Position.t -> pp:PPTree.t -> S.ident -> T.scheme -> Name.t

(** Translate a scheme to an internal representation. *)
val tr_scheme : pos:Position.t -> pp:PPTree.t -> T.scheme -> Name.scheme

(** Apply renaming to a name *)
val rename : T.Ren.t -> Name.t -> Name.t

(** Apply renaming to a named pattern *)
val rename_pattern : T.Ren.t -> Name.pattern -> Name.pattern
