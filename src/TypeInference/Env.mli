(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

open Common

(** Type of function that is called on each use of the variable *)
type on_use = Position.t -> unit

type t

(** Initial environment *)
val initial : t

(** Extend the environment with a named type variable. In opposite to
  [add_tvar], this function does not create a fresh variable, but uses
  an existing one. Provided type variable must be fresh enough, i.e., it
  should not be present in the environment's scope. *)
val add_existing_tvar :
  ?pos:Position.t -> ?public:bool -> ?on_use:on_use ->
    t -> S.tvar -> T.tvar -> t

(** Extend the environment with an anonymous type variable. In opposite to
  [add_anon_tvar], this function does not create a fresh variable, but uses
  an existing one. Provided type variable must be fresh enough, i.e., it
  should not be present in the environment's scope. *)
val add_existing_anon_tvar :
  ?pos:Position.t -> ?name:string -> t -> T.tvar -> t

(** Extend the environment with a named type variable. The optional position
  should point to the place of binding in the source code. *)
val add_tvar :
  ?pos:Position.t -> ?public:bool -> ?on_use:on_use ->
    t -> S.tvar -> T.kind -> t * T.tvar

(** Extend the environment with an anonymous type variable. The optional
  position should point to the place of binding in the source code. The
  optional name is used for pretty-printing purposes. *)
val add_anon_tvar :
  ?pos:Position.t -> ?name:string -> t -> T.kind -> t * T.tvar

(** Extend the environment with an alias for an existing type variable.
  This type variable should be present in the environment's scope *)
val add_tvar_alias :
  ?pos:Position.t -> ?public:bool -> t -> S.tvar -> T.tvar -> t

(** Extend the environment with an existing variable of given scheme *)
val add_existing_var :
  ?public:bool -> ?on_use:on_use -> t -> S.var -> T.var -> T.scheme -> t

(** Extend the environment with an existing implicit of given scheme *)
val add_existing_implicit :
  ?public:bool -> ?on_use:on_use -> t -> S.iname -> T.var -> T.scheme -> t

(** Extend the environment with an existing variable representing a method *)
val add_existing_method :
  ?public:bool -> ?on_use:on_use -> t ->
    T.tvar -> S.method_name -> T.var -> T.scheme -> t

(** Extend the environment with a polymorphic variable *)
val add_var :
  ?public:bool -> ?on_use:on_use -> t -> S.var -> T.scheme -> t * T.var

(** Extend the environment with a polymorphic named implicit. *)
val add_implicit :
  ?public:bool -> ?on_use:on_use -> t -> S.iname -> T.scheme -> t * T.var

(** Extend the environment with a monomorphic "label" implicit. Usually, it has
  a label type, but it must be stated explicitly, e.g., by
  [add_the_label env (T.Type.t_label tp)] *)
val add_the_label : t -> T.typ -> t * T.var

(** Add a method associated with given type variable (owner). Method must have
  arrow type, where the head type variable of an argument is the same
  as the owner *)
val add_method :
  ?public:bool -> ?on_use:on_use ->
  t -> T.tvar -> S.method_name -> T.scheme -> t * T.var

(** Extend the environment with information that given identifier when used
  as function is a method of given name. *)
val add_method_fn : public:bool -> t -> S.var -> S.method_name -> t

(** Assign ADT definition to given type variable. For abstract datatype,
  the [public] flag should be set to [false]. *)
val add_adt : ?public:bool -> t -> T.tvar -> Module.adt_info -> t

(** Add constructor of given name and index to the environment *)
val add_ctor : ?public:bool -> t -> string -> int -> Module.adt_info -> t

(** Create a new module on top of the module stack. *)
val enter_module : t -> t

(** Finalize a module definition and add it to the outer module with the
  given name. *)
val leave_module : public:bool -> t -> S.module_name -> t

(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : public:bool -> t -> Module.t -> t

(** Lookup for Unif representation of a type variable. Returns [None] if
  variable is not bound. *)
val lookup_tvar : t -> S.tvar -> (T.typ * on_use) option

(** Lookup for variable-like identifier. Returns [None] if variable is not
  bound. *)
val lookup_var : t -> S.var -> (Module.var_info * on_use) option

(** Lookup for Unif representation, a scheme, and "on-use" function of a named
  implicit. Returns [None] if implicit is not bound. *)
val lookup_implicit :
  t -> S.iname -> (T.var * T.scheme * on_use) option

(** Lookup for implicit "label" variable. *)
val lookup_the_label : t -> (T.var * T.scheme * on_use) option

(** Lookup for method associated with given type variable *)
val lookup_method :
  t -> T.tvar -> S.method_name -> (T.var * T.scheme * on_use) option

(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT *)
val lookup_ctor : t -> S.ctor_name -> (int * Module.adt_info) option

(** Lookup for ADT definition assigned for given type variable *)
val lookup_adt : t -> T.tvar -> Module.adt_info option

(** Lookup for a module of the given name. *)
val lookup_module : t -> S.module_name -> Module.t option

(** Increase the level of the environment's scope. The level should be
  increased for each place, when implicit type generalization can occur. *)
val incr_level : t -> t

(** Get the current scope *)
val scope : t -> T.scope

(** Get the level of the environment *)
val level : t -> int

(** Get the type pretty-printing tree *)
val pp_tree : t -> PPTree.t

(** Create a fresh unification variable in the current scope *)
val fresh_uvar : t -> T.kind -> T.typ
