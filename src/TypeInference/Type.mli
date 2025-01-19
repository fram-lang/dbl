(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kind-checking and translation of type expressions *)

open Common

(** Translate a kind expression *)
val tr_kind : S.kind_expr -> T.kind

(** Translate a type expression and infer its kind *)
val infer_kind : 'st Env.t -> S.type_expr -> T.type_expr * T.kind

(** Check kind and translate a type expression *)
val check_kind : 'st Env.t -> S.type_expr -> T.kind -> T.type_expr

(** Check and translate a type-scheme expression *)
val tr_scheme : 'st Env.t -> S.scheme_expr -> T.scheme_expr

(** Translate scheme named parameters and split it into type and value
  parameters. The optional [data_targs] parameter is a list of type
  parameters of a datatype in case of translation of named parameters of
  the constructor: named types introduced by a datatype cannot be the same
  as names introduce by a constructor. *)
val tr_scheme_args : ?data_targs: T.named_tvar list ->
  'st Env.t -> S.scheme_arg list ->
    'st Env.t * T.named_tvar list * T.named_scheme_expr list

(** Check kind and translate a type expression of kind Type *)
val tr_ttype : 'st Env.t -> S.type_expr -> T.type_expr

(** Check if given type argument has given kind. Returns extended environment,
  the introduced external type name, and freshly bound type variable. *)
val check_type_arg : 'st Env.t -> S.type_arg -> T.kind ->
  'st Env.t * T.tname * T.tvar

(** Translate formal type parameters and extend the environment *)
val tr_named_type_args :
  'st Env.t -> S.named_type_arg list -> 'st Env.t * T.named_tvar list
