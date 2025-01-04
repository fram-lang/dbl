(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving named parameters *)

open Common

(** List of variables that could be reinstantiated. *)
type reinst_list

(** Empty reinstantiation list *)
val no_reinst : reinst_list

(** Imlicitly introduce all variables and named parameters, bound by given
  scheme, to the environment. Returns extended environment, list of variables
  that could be reinstantiated, list of type variables, list of introduced
  named parameters, and the body of the scheme. *)
val open_scheme : pos:Position.t -> Env.t -> T.scheme ->
  Env.t * reinst_list * T.tvar list * (T.var * T.scheme) list * T.typ

(** Implicitly introduce all variables and named parameters in case of
  explicit binding of named parameters. Returns extended environment, list of
  type variables, list of introduced named parameters, and the body of the
  scheme. *)
val open_scheme_explicit : pos:Position.t -> Env.t -> T.scheme ->
  Env.t * (T.tname * T.tvar) list * (T.name * T.var * T.scheme) list * T.typ

(** Implicitly instantiate all parameters in given polymorphic expression of
  given scheme. Returns the instantiated expression, its type, and the list of
  generated constraints. If the expression is instantiated immediately after a
  generalization, the reinstantiation list should should contain generalized
  variables (it should be a list returned by [open_scheme]). *)
val instantiate :
  pos:Position.t -> Env.t -> reinst_list -> T.poly_expr -> T.scheme ->
    T.expr * T.typ * Constr.t list

(** Coerce a named parameter (polymorphic expression) of given scheme to an
  another scheme. Returns coerced expression and the list of generated
  constraints. This function is basically a combination of [open_scheme]
  followed by [instantiate]. *)
val coerce_scheme :
  pos:Position.t -> name:T.name ->
  Env.t -> T.poly_expr -> T.scheme -> T.scheme -> T.poly_expr * Constr.t list

(** Resolve an implicit parameter of given scheme in given environment. *)
val resolve_implicit :
  pos:Position.t -> Env.t -> S.iname -> T.scheme ->
    T.poly_expr * Constr.t list

(** Resolve a method parameter of given scheme in given environment. *)
val resolve_method :
  pos:Position.t -> Env.t -> S.method_name -> T.scheme ->
    T.poly_expr * Constr.t list
