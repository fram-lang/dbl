(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)
(*
open Common

(** Make polymorphic function with given type parameters *)
val make_tfun : T.named_tvar list -> T.expr -> T.expr

(** Make function polymorphic in named parameters *)
val make_nfun : (T.name * T.var * T.scheme) list -> T.expr -> T.expr

(** Generate a type application to given list of types *)
val make_tapp : T.expr -> T.typ list -> T.expr

(** Generalize type to polymorphic scheme. The first parameter is a list
  of type parameters (explicit or implicitly introduced by
  [ImplicitEnv.end_generalize_pure], and the second parameter is a list of
  implicit parameters. *)
val generalize :
  pos:Position.t ->
  T.named_tvar list -> (T.name * T.var * T.scheme) list ->
  T.expr -> T.typ -> T.expr * T.scheme

(** Guess types used to instantiate polymorphic function. Some of these types
  may be provided by optional [tinst] or [hints] parameter. The [hints]
  parameter maps type variables from the last parameter to types. If a type
  is povided by both [tinsts] and [hints] parameter, the first one has higher
  priority. Returns substitution from given type variables to guessed types
  together with list of these types. *)
val guess_types :
  pos:Position.t ->
  Env.t -> ?tinst:(T.tname * T.typ) list -> ?hints:(T.typ T.TVar.Map.t) ->
  T.named_tvar list ->
    T.subst * T.typ list

(** Instantiate named parameters of polymorphic expression. It takes possibly
  empty list of explicit instantiations. These instantiations are pure, so
  their order doesn't matter. *)
val instantiate_named_params :
  Env.t -> T.expr -> T.named_scheme list -> (T.name * T.expr) list -> T.expr

(** Create a function that represents ADT constructor of given index,
  not applied to any parameters yet, even the type parameters of the ADT. *)
val ctor_func : pos:Position.t -> int -> Module.adt_info -> T.expr

(** Extend an expression with pattern-matching on argument or other form
  of binder. In call [arg_match pat body tp eff] the meaning of the parameters
  is the following:
  - [pat]  -- the binding pattern;
  - [body] -- an expression that should be extended with a pattern-matching,
      e.g. body of a function;
  - [tp]   -- the type of [body] expression;
  - [eff]  -- the optional effect of [body] expression. [None] means that both
    body and pattern-matching is pure.
  It returns a variable that should be bound instead of pattern together with
  an extended expression. *)
val arg_match :
  T.pattern -> T.expr -> T.typ -> T.effrow option -> T.var * T.expr

(** Same as [arg_match], but take multiple binders of named parameters. *)
val inst_args_match :
  (T.name * T.pattern * T.scheme) list -> T.expr -> T.typ -> T.effrow option ->
    (T.name * T.var * T.scheme) list * T.expr
*)
