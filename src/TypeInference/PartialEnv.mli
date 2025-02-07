(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Partial environments created by patterns. *)

open Common

type t

(** Empty environment *)
val empty : t

(** Singleton environment with a single alias for a type variable. In contrast
  to [singleton_tvar], given type variable should already exist in the
  environment. *)
val singleton_tvar_alias :
  public:S.is_public -> pos:Position.t -> S.tvar -> T.tvar -> t

(** Singleton environment with a single value. *)
val singleton_val :
  public:S.is_public -> pos:Position.t -> Name.t -> T.var -> T.scheme -> t

(** Singleton environment with a single module. Provided type variables should
  already exist in the environment. *)
val singleton_module :
  public:S.is_public ->
  pos:Position.t ->
  types:(S.tvar * T.tvar) list ->
  vals:(Name.t * T.var * T.scheme) list ->
    S.module_name -> t

(** Add an anonymous type variable to the environment *)
val add_anon_tvar : pos:Position.t -> t -> T.tvar -> t

(** Add an alias for a type variable to the environment *)
val add_tvar_alias :
  public:S.is_public -> pos:Position.t -> t -> S.tvar -> T.tvar -> t

(** Add existing variable to the environment. It cannot shadow an existing
  variables. *)
val add_var :
  public:S.is_public -> pos:Position.t -> pp:PPTree.t ->
    t -> S.var -> T.var -> T.scheme -> t

(** Add existing implicit variable to the environment. It cannot shadow an
  existing variables. *)
val add_implicit :
  public:S.is_public -> pos:Position.t -> pp:PPTree.t ->
    t -> S.iname -> T.var -> T.scheme -> t

(** Add a method (the owner must exists) to the environment. It cannot shadow
  an existing methods. *)
val add_method :
  public:S.is_public -> pos:Position.t -> pp:PPTree.t ->
    t -> Name.method_owner -> S.method_name -> T.var -> T.scheme -> t

(** Join two partial environments, taking the union. The same variable cannot
  be bound in both environments. *)
val join : pp:PPTree.t -> t -> t -> t

(** Enter a new scope (twice) and add all members of the partial environment
  to the environment. The second parameter is a list of type parameters bound
  when the partial environment was created. Type aliases to these parameters
  may apper in the partial envrionment, so these type parameters are added to
  the environment. Returns tuple containing the following:
  - the new environment
  - the scope that contains type parameters, but does not contain other types
    introduced by the partial environment
  - refresh version of type parameters (the second argument)
  - the renaming of variables introduced by the partial environment. *)
val extend : 'st Env.t -> type_param list -> t -> 
  'st Env.t * Scope.t * T.named_tvar list * T.Ren.t
