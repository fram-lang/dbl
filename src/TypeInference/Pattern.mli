(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

open Common

(* Check type of given pattern. Returns partial-environment (variables bound
  by this pattern), translated pattern, and the effect of pattern-matching. *)
val check_type : 'st Env.t -> S.pattern -> T.typ ->
  PartialEnv.t * T.pattern * T.effct

(** Infer scheme expression of given pattern. Returns partial-environment
  (variables bound by this pattern), translated pattern, its scheme
  expression, and the effect of pattern-matching *)
val infer_scheme : 'st Env.t -> S.pattern ->
  PartialEnv.t * T.pattern * T.scheme_expr * T.effct

(** Enter a new scope and translate a list of named patterns. The returned
  environment contains type parameters, but not the type variables bound by
  value patterns. On the other hand, returned partial environment defines
  type parameters as aliases for type variables added to the environment. *)
val infer_named_patterns :
  'st Env.t -> S.named_pattern list ->
    'st Env.t * PartialEnv.t *
      type_param list * Name.pattern list * T.effct

(** Enter a new scope and infer type-scheme of given pattern. Returns extended
  environment, translated pattern, its scheme, and the effect of
  pattern-matching *)
val infer_scheme_ext :
  'st Env.t -> S.pattern -> 'st Env.t * T.pattern * T.scheme_expr * T.effct

(** Check if given pattern has given type scheme. Returns extended
  environment, translated pattern, and the effect of pattern-matching *)
val check_scheme_ext :
  'st Env.t -> S.pattern -> T.scheme -> 'st Env.t * T.pattern * T.effct

(** Check if given pattern has given type. Returns extended environment,
  translated pattern, and the effect of pattern-matching *)
val check_type_ext :
  'st Env.t -> S.pattern -> T.typ -> 'st Env.t * T.pattern * T.effct

(** Check schemes of named patterns. These parameters should be already
  introduced in the environment, and provided as the last two arguments.
  The environment is extended with aliases for these parameters. *)
val check_named_patterns_ext :
  pos:Position.t ->
  'st Env.t -> S.named_pattern list ->
  T.named_tvar list -> (T.name * T.var * T.scheme) list ->
    'st Env.t * (T.var * T.pattern) list * T.effct

(** Translate a list of named patterns and extend the environment. The
  returned scope is a scope of the environment extended with type parameters,
  but not with the type variables bound by value patterns. *)
val infer_named_patterns_ext :
  'st Env.t -> S.named_pattern list ->
    'st Env.t * Scope.t * T.named_tvar list * Name.pattern list * T.effct
