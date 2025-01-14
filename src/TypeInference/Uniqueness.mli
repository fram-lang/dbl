(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

open Common

(** Unique representation of a parameter name *)
type unique_name =
  | UNVar         of string
  | UNOptionalVar of string
  | UNImplicit    of string
  | UNMethod      of T.tvar * string
(* TODO: this definition should be moved to Unif. It might be useful in other
   places as well. *)

(** Translate a name to Unif representation *)
val tr_name : unique_name -> T.name

(** Ensure that each constructor in given ADT has a unique name *)
val check_ctor_uniqueness : S.ctor_decl list -> unit

(** Ensure that checked named type parameters are unique *)
val check_unif_named_type_args : (Position.t * T.tname * T.tvar) list -> unit

(** Ensure that checked named parameters are unique *)
val check_names : env:Env.t -> (Position.t * unique_name) list -> unit

(** Ensure that type names introduced by the first parameter are unique and
  do not collide with the second parameter *)
val check_generalized_types :
  pos:Position.t -> T.named_tvar list -> T.named_tvar list -> unit

(** Ensure that names introduced by the first parameter are unique and do not
  collide with the second parameter. This function assumes that well-formedness
  of method schemes has been checked. *)
val check_generalized_names : pos:Position.t -> env:Env.t ->
  (T.name * T.var * T.scheme_expr) list -> T.named_scheme list -> unit
