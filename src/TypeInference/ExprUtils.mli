(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Make polymorphic function with given type parameters *)
val make_tfun : T.tvar list -> T.expr -> T.expr

(** Make function polymorphic in implicit parameters *)
val make_ifun : (S.name * T.var * T.scheme) list -> T.expr -> T.expr

(** Generate a type application to given list of types *)
val make_tapp : T.expr -> T.typ list -> T.expr

(** Generalize type to polymorphic scheme. The second parameter is a list
  of explicit type parameters, and the third parameter is a list of
  implicit parameters. *)
val generalize : Env.t -> T.tvar list -> (S.name * T.var * T.scheme) list ->
  T.expr -> T.typ -> T.expr * T.scheme

(** Guess types used to instantiate polymorphic function. Returns substitution
  from given type variables to guessed types together with list of these types.
  *)
val guess_types : Env.t -> T.tvar list -> T.subst * T.typ list

(** Instantiate named parameters of polymorphic expression. It takes possibly
  empty list of explicit instantiations. These instantiations are pure, so
  their order doesn't matter. *)
val instantiate_implicits :
  Env.t -> T.expr -> (T.name * T.scheme) list -> (S.name * T.expr) list -> T.expr

(** Create a function that represents ADT constructor of given index,
  not applied to any parameters yet, even the type parameters of the ADT. *)
val ctor_func : pos:Position.t -> int -> Env.adt_info -> T.expr

(** Extend an expression with pattern-matching on argument or other form
  of binder. In call [arg_match pat body tp eff] the meaning of the parameters
  is the following:
  - [pat]  -- the binding pattern;
  - [body] -- an expression that should be extended with a pattern-matching,
      e.g. body of a function;
  - [tp]   -- the type of [body] expression;
  - [eff]  -- the effect of [body] expression.
  It returns a variable that should be bound instead of pattern together with
  an extended expression. *)
val arg_match : T.pattern -> T.expr -> T.typ -> T.effect -> T.var * T.expr

(** Same as [arg_match], but take multiple binders of named parameters. *)
val inst_args_match :
  (S.name * T.pattern * T.scheme) list -> T.expr -> T.typ -> T.effect ->
    (S.name * T.var * T.scheme) list * T.expr
