(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on built-in types. *)

open Common

(** Built [Option] type with given type argument, and transform it to a
  monomorphic scheme. *)
val mk_option_scheme : T.typ -> T.scheme

(** Treat given scheme as an [Option] type and extract the type argument. *)
val scheme_to_option_arg : T.scheme -> T.typ

(** Create a [None] constructor *)
val mk_none : pos:Position.t -> T.typ -> T.expr

(** Wrap a polymorphic expression (of a monomorphic scheme) with [Some]
  constructor. *)
val mk_some_poly : pos:Position.t -> T.typ -> T.poly_expr -> T.expr
