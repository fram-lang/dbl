(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

open Common

(** Environments indexed with their current state. See [Common] module for
  details. *)
type 'st t

(** Initial environment *)
val initial : closed t

(* ========================================================================= *)

(** Extend the environment with a named type variable. The optional position
  should point to the place of binding in the source code. *)
val add_tvar :
  ?pos:Position.t -> ?public:bool ->
    'st t -> S.tvar -> T.kind -> 'st t * T.tvar

(** Extend the environment with an anonymous type variable. The optional
  position should point to the place of binding in the source code. The
  optional name is used for pretty-printing purposes. *)
val add_anon_tvar :
  ?pos:Position.t -> ?pp_uid:PPTree.uid -> ?name:string ->
    'st t -> T.kind -> 'st t * T.tvar

(** Extend the environment with an alias for an existing type variable.
  This type variable should be present in the environment's scope *)
val add_tvar_alias :
  ?pos:Position.t -> ?public:bool ->
    'st t -> S.tvar -> T.tvar -> 'st t

(** Extend the environment with a type alias. The optional position
  should point to the place of binding in the source code. *)
val add_type_alias :
  ?pos:Position.t -> ?public:bool ->
    'st t -> S.tvar -> T.typ -> 'st t * T.ty_alias

(** Extend the environment with a polymorphic value *)
val add_val :
  ?public:bool -> 'st t -> Name.t -> T.scheme -> 'st t * T.var

(** Extend the environment with a polymorphic named implicit. *)
val add_implicit :
  ?public:bool -> 'st t -> S.iname -> T.scheme -> 'st t * T.var

(** Extend the environment with a monomorphic "label" implicit. Usually, it has
  a label type, but it must be stated explicitly, e.g., by
  [add_the_label env (T.Type.t_label tp)] *)
val add_the_label : 'st t -> T.typ -> 'st t * T.var

(** Add a method associated with given type (owner). The method must have
  an arrow type, where the head type variable of an argument is the same
  as the owner *)
val add_method :
  ?public:bool ->
    'st t -> Name.method_owner -> S.method_name -> T.scheme -> 'st t * T.var

(** Assign ADT definition to given type variable. For abstract datatype,
  the [public] flag should be set to [false]. *)
val add_adt : ?public:bool -> 'st t -> T.tvar -> Module.adt_info -> 'st t

(** Add constructor of given name and index to the environment *)
val add_ctor :
  ?public:bool -> 'st t -> string -> int -> Module.adt_info -> 'st t

(* ========================================================================= *)

(** Enter a new scope *)
val enter_scope : 'st t -> 'st t * Scope.t

(** Enter a new section of parameter declarations. *)
val enter_section : 'st t -> ('st, sec) opn t

(** Leave the section of parameter declarations *)
val leave_section : ('st, sec) opn t -> 'st t

(** Extend environment with a declaration of a type parameter *)
val declare_type : pos:Position.t ->
  ('st, sec) opn t -> T.tname -> S.tvar -> T.kind -> ('st, sec) opn t

(** Extend environment with a declaration of a value parameter. The meaning of
  parameters is the following.
  - [free_types] -- type variables that occurs freely in the scheme of this
    parameter and should be separately instantiated for each use.
  - [used_types] -- previously declared type UIDs, together with the type
    variables used as them in the scheme of this parameter.
  - [name] -- the name of the parameter that will be visible in the
    generalized scheme.
  - [local_name] -- then name of the parameter used in the environment. *)
val declare_val : pos:Position.t -> ('st, sec) opn t ->
  free_types:T.tvar list -> used_types:(UID.t * T.tvar) list ->
    name:Name.t -> local_name:Name.t -> T.scheme_expr -> ('st, sec) opn t

(** Enter the scope where declared parameters are visible. Returns a list of
  parameters that should be passed to one of [end_generalize_*] functions
  when the scope is left. These functions are defined in [ParamGen] module. *)
val begin_generalize : ('st, sec) opn t -> exp t * ParamEnv.param_list

(** Try to access type parameter and return its status. *)
val check_type_param : pos:Position.t ->
  'st t -> UID.t -> (T.tname, T.tvar) ParamEnv.param_status

(** Try to access value parameter and return its status. *)
val check_val_param : pos:Position.t ->
  'st t -> UID.t -> (Name.t, T.var * T.scheme) ParamEnv.param_status

(* ========================================================================= *)

(** Create a new module on top of the module stack. *)
val enter_module : 'st t -> ('st, modl) opn t

(** Finalize a module definition and add it to the outer module with the
  given name. *)
val leave_module : public:bool -> ('st, modl) opn t -> S.module_name -> 'st t

(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : public:bool -> 'st t -> closed Module.t -> 'st t

(* ========================================================================= *)

(** Lookup for Unif representation of a type variable. *)
val lookup_tvar : 'st t -> S.tvar -> Module.type_info option

(** Lookup for a value with given name. *)
val lookup_val : 'st t -> Name.t -> Module.val_info option

(** Lookup for variable-like identifier. *)
val lookup_var : 'st t -> S.var -> Module.val_info option

(** Lookup for implicit value *)
val lookup_implicit : 'st t -> S.iname -> Module.val_info option

(** Lookup for implicit "label" variable. *)
val lookup_the_label : 'st t -> Module.val_info option

(** Lookup for method associated with given owner *)
val lookup_method :
  'st t -> Name.method_owner -> S.iname -> Module.val_info option

(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT *)
val lookup_ctor : 'st t -> S.ctor_name -> (int * Module.adt_info) option

(** Lookup for ADT definition assigned for given type variable *)
val lookup_adt : 'st t -> T.tvar -> Module.adt_info option

(** Lookup for a module of the given name. *)
val lookup_module : 'st t -> S.module_name -> closed Module.t option

(* ========================================================================= *)

(** Get the current scope *)
val scope : 'st t -> Scope.t

(** Get the type pretty-printing tree *)
val pp_tree : 'st t -> PPTree.t

(** Create a fresh unification variable in the current scope *)
val fresh_uvar : pos:Position.t -> 'st t -> T.kind -> T.typ
