(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

open Common

(** Generalize additional parameters of polymorphic expression of given
  scheme. *)
val generalize : pos:Position.t -> pp:PPTree.t ->
  T.named_tvar list -> (Name.t * T.var * T.scheme_expr) list ->
    T.poly_expr -> Name.scheme -> T.poly_expr * T.scheme

(** Create a function that represents ADT constructor of given index,
  not applied to any parameters yet, even the type parameters of the ADT. *)
val ctor_func : pos:Position.t -> int -> Module.adt_info -> T.poly_expr

(** Extend an expression with pattern-matching on argument or other form
  of binder. In call [arg_match pat body tp eff] the meaning of the parameters
  is the following:
  - [pat]  -- the binding pattern;
  - [body] -- an expression that should be extended with a pattern-matching,
      e.g. body of a function;
  - [tp]   -- the type of [body] expression;
  - [eff]  -- the effect of the whole expression.
  It returns a variable that should be bound instead of pattern together with
  an extended expression. *)
val match_var :
  T.pattern -> T.expr -> T.typ -> T.effct -> T.var * T.expr

(** Extend an expression with pattern-matching on a list of arguments. The
  function is similar to [match_var], but it requires that variables are
  already generated and provided together with patterns. *)
val match_args :
  (T.var * T.pattern) list -> T.expr -> T.typ -> T.effct -> T.expr
