(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

open Common

(** Ensure that each constructor in given ADT has a unique name *)
val check_ctor_uniqueness : S.ctor_decl list -> unit

(** Ensure that checked named type parameters are unique *)
val check_unif_named_type_args : (Position.t * T.tname * T.tvar) list -> unit

(** Ensure that checked named parameters are unique *)
val check_names : pp:PPTree.t -> (Position.t * Name.t) list -> unit

(** Ensure that type names introduced by the first parameter are unique and
  do not collide with the second parameter *)
val check_generalized_types :
  pos:Position.t -> T.named_tvar list -> T.named_tvar list -> unit

(** Ensure that names introduced by the first parameter are unique and do not
  collide with the second parameter. *)
val check_generalized_names : pos:Position.t -> pp:PPTree.t ->
  (Name.t * T.var * T.scheme_expr) list -> (Name.t * T.scheme) list -> unit
