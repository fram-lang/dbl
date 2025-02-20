(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

open Common

(** Internal represenation of list of constructors. *)
type ctor_decl_list

(** Get the kind of ADT that has given its list of parameters *)
val kind : T.named_tvar list -> T.kind

(** Check well-formedness of ADT constructors. The [data_targs] parameter is a
  list of parameters of the datatype. The evironment should contain these
  parameters. *)
val check_ctor_decls :
  data_targs:T.named_tvar list ->
  Env.t -> S.ctor_decl list -> ctor_decl_list

(** Finalize checking of ADT definition. It extends environment by ADT
  metadata (proof) and constructors. The [nonrec_scope] parameter is a scope
  not extended with (mutually-)recursive definitions, and is used to check if
  a type is strictly positively recursive. Note that during type inference,
  the type is considered as strictly positively recursive if it does not
  contain type variables on non-strictly positive position or unification
  variables that do not fit in this scope. This approach may lead to some
  false-negatives, if the programmer omit some types that do not contain any
  recursive occurrences, e.g.,
  [ data rec T = C of (_ -> T) in ... C (fn (x : Int) => ...) ]. *)
val finalize_check :
  nonrec_scope:T.scope ->
  Env.t -> T.tvar -> name:S.tvar -> T.named_tvar list -> ctor_decl_list ->
    Env.t * T.data_def
