(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Ensure that each constructor in given ADT has a unique name *)
val check_ctor_uniqueness : S.ctor_decl list -> unit

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
