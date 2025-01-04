(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

open Common

(* Check type of given pattern. Returns partial-environment (variables bound
  by this pattern), translated pattern, and the effect of pattern-matching. *)
val check_type : Env.t -> S.pattern -> T.typ ->
  PartialEnv.t * T.pattern * T.effect
(*
(** Check if given pattern fits in given scope and has given type.
  Returns extended environment, translated pattern, set of bound names (as
  a map from names, to binding occurrence), and the effect of
  pattern-matching (matching against ADT constructors is impure). The scope is
  passed separately from environment, since the scope of the pattern flows
  top-down in a complex patterns, while the environment flows left-to-right.
  *)
val check_type :
  env:Env.t -> scope:T.scope -> S.pattern -> T.typ ->
    Env.t * T.pattern * Position.t T.Name.Map.t * ret_effect
*)
(** Infer type-scheme of given pattern. Returns extended environment,
  translated pattern, its scheme, and the effect of pattern-matching *)
val infer_scheme_ext :
  Env.t -> S.pattern -> Env.t * T.pattern * T.scheme * T.effect

(** Check if given pattern has given type scheme. Returns extended
  environment, translated pattern, and the effect of pattern-matching *)
val check_scheme_ext :
  Env.t -> S.pattern -> T.scheme -> Env.t * T.pattern * T.effect

(** Check if given pattern has given type. Returns extended environment,
  translated pattern, and the effect of pattern-matching *)
val check_type_ext :
  Env.t -> S.pattern -> T.typ -> Env.t * T.pattern * T.effect
(*
(** Check if given argument has given type scheme. Returns extended
  environment, translated argument as pattern, and the effect of
  pattern-matching *)
val check_arg_scheme :
  Env.t -> S.arg -> T.scheme -> Env.t * T.pattern * ret_effect

(** Infer type schemes of given named formal parameters. Returns extended
  environment, the effect of pattern-matching, and the list of translated
  parameters as triples: name, pattern, and its type scheme *)
val infer_named_arg_schemes :
  Env.t -> S.named_arg list ->
    Env.t * (T.name * T.pattern * T.scheme) list * ret_effect
*)

(** Check schemes of named patterns. These parameters should be already
  introduced in the environment, and provided as the last two arguments.
  The environment is extended with aliases for these parameters. *)
val check_named_patterns_ext :
  Env.t -> S.named_pattern list ->
  T.named_tvar list -> (T.name * T.var * T.scheme) list ->
    Env.t * (T.var * T.pattern) list * T.effect

(** Translate a list of named patterns and extend the environment. The
  returned scope is a scope of the environment extended with type parameters,
  but not with the type variables bound by value patterns. *)
val infer_named_patterns_ext :
  Env.t -> S.named_pattern list ->
    Env.t * T.scope *
      T.named_tvar list * (T.name * T.pattern * T.scheme) list * T.effect
