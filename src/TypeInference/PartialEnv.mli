(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Partial environments created by patterns. *)

open Common

type t

(** Empty environment *)
val empty : t

(** Singleton environment with single type variable. Provided Unif type
  variable should be fresh enough. *)
val singleton_tvar :
  public:S.is_public -> pos:Position.t -> S.tvar -> T.tvar -> t

(** Singleton environment with single alias for a type variable. In contrast to
  [singleton_tvar], given type variable should already exist in the
  environment. *)
val singleton_tvar_alias :
  public:S.is_public -> pos:Position.t -> S.tvar -> T.tvar -> t

(** Singleton environment with single variable *)
val singleton_var :
  public:S.is_public -> pos:Position.t -> S.var -> T.scheme -> t * T.var

(** Singleton environment with single implicit variable *)
val singleton_implicit :
  public:S.is_public -> pos:Position.t -> S.iname -> T.scheme -> t * T.var

(** Singleton environment with single method *)
val singleton_method :
  public:S.is_public -> pos:Position.t ->
    T.tvar -> S.method_name -> T.scheme -> t * T.var

(** Singleton environment with single module *)
val singleton_module :
  public:S.is_public ->
  pos:Position.t ->
  types:(S.tvar * T.tvar) list ->
  vars:(S.var * T.var * T.scheme) list ->
  implicits:(S.iname * T.var * T.scheme) list ->
  methods:(T.tvar * S.method_name * T.var * T.scheme) list ->
    S.module_name -> t

(** Add an anonymous type variable to the environment *)
val add_anon_tvar : pos:Position.t -> t -> T.tvar -> t

(** Add an alias for a type variable to the environment *)
val add_tvar_alias :
  public:S.is_public -> pos:Position.t -> t -> S.tvar -> T.tvar -> t

(** Add existing variable to the environment. It cannot shadow an existing
  variables. *)
val add_var :
  public:S.is_public -> pos:Position.t -> t -> S.var -> T.var -> T.scheme -> t

(** Add existing implicit variable to the environment. It cannot shadow an
  existing variables. *)
val add_implicit :
  public:S.is_public -> pos:Position.t ->
    t -> S.iname -> T.var -> T.scheme -> t

(** Add a method (bound to existing variable) to the environment. It cannot
  shadow an existing methods. *)
val add_method :
  public:S.is_public -> pos:Position.t ->
    t -> T.tvar -> S.method_name -> T.var -> T.scheme -> t

(** Join two partial environments, taking the union. The same variable cannot
  be bound in both environments. *)
val join : t -> t -> t

(** Add all members of a partial environment to the environment. *)
val extend : Env.t -> t -> Env.t
