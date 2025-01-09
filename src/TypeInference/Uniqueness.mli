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

(** Translate a name to Unif representation *)
val tr_name : unique_name -> T.name

(** Ensure that each constructor in given ADT has a unique name *)
val check_ctor_uniqueness : S.ctor_decl list -> unit
(*
(** Ensure that each named type parameter is instantiated at most once *)
val check_type_inst_uniqueness : S.type_inst list -> unit

(** Ensure that each named parameter is instantiated at most once *)
val check_inst_uniqueness : S.inst list -> unit

(** Ensure that each named type is bound at most once *)
val check_named_type_arg_uniqueness : S.named_type_arg list -> unit

(** Ensure that each named pattern is defined at most once *)
val check_named_pattern_uniqueness : S.named_pattern list -> unit

(** Ensure that names of type bound by a constructor do not collide with
  names bound by datatype *)
val check_ctor_named_types : T.named_tvar list -> S.named_type_arg list -> unit

(** Ensure that generalized names are unique *)
val check_generalized_named_types : pos:Position.t -> T.named_tvar list -> unit
*)

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
