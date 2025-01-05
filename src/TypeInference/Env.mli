(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

open Common

(** Type of function that is called on each use of the variable *)
type on_use = Position.t -> unit

type t
(*
(** Additional information about type variables used to pretty printing *)
type pp_info = {
  pp_base_name : string;
    (** Default name used as a base for a name generation *)

  pp_names     : S.tvar S.path list;
    (** List of names assigned to a type variable (empty for anonymous types) *)

  pp_pos       : Position.t option
    (** Position of the binding place *)
}
*)
(** Empty environment *)
val empty : t
(*
(** Extend an environment with a polymorphic variable *)
val add_poly_var : ?public:bool -> t -> S.var -> T.scheme -> t * T.var

(** Extend an environment with a monomorphic variable *)
val add_mono_var : ?public:bool -> t -> S.var -> T.typ -> t * T.var
*)
(** Extend an environment with a polymorphic named implicit. *)
val add_poly_implicit :
  ?public:bool -> ?on_use:on_use -> t -> S.iname -> T.scheme -> t * T.var
(*
(** Extend an environment with a monomorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_mono_implicit :
  ?public:bool -> t -> S.iname -> T.typ -> (Position.t -> unit) -> t * T.var
*)
(** Extend an environment with a monomorphic "label" implicit. Usually, it has
  a label type, but it must be stated explicitly, e.g., by
  [add_the_label env (T.Type.t_label tp)] *)
val add_the_label : t -> T.typ -> t * T.var
(*
(** Extend an environment with information that given identifier when used
  as function is a method of given name. *)
val add_method_fn : public:bool -> t -> S.var -> S.method_name -> t

(** Extend an environment with a named type variable. The optional position
  should point to the place of binding in the source code. *)
val add_tvar :
  ?pos:Position.t -> ?public:bool -> t -> S.tvar -> T.kind -> t * T.tvar

(** Extend an environment with a type variable labeled with "effect". Such
  a type always have [effect] kind. The optional position should point to
  the place of binding in the source code. *)
val add_the_effect : ?pos:Position.t -> t -> t * T.tvar
*)
(** Extend an environment with an anonymous type variable. The optional
  position should point to the place of binding in the source code. The
  optional name is used for pretty-printing purposes. *)
val add_anon_tvar :
  ?pos:Position.t -> ?name:string -> t -> T.kind -> t * T.tvar
(*
(** Extend an environment with a type alias. *)
val add_type_alias : ?public:bool -> t -> S.tvar -> T.typ -> t

(** Extend an environment with an alias labeled with "effect". Given type
  must have the [effect] kind. *)
val add_the_effect_alias : t -> T.typ -> t

(** Assign ADT definition to given type variable. *)
val add_data : t -> T.tvar -> Module.adt_info -> t

(** Add constructor of given name and index to the environment *)
val add_ctor : ?public:bool -> t -> string -> int -> Module.adt_info -> t
*)
(** Add a method associated with given type variable (owner). Method must have
  arrow type, where the head type variable of an argument is the same
  as the owner *)
val add_poly_method :
  ?public:bool -> ?on_use:on_use ->
  t -> T.tvar -> S.method_name -> T.scheme -> t * T.var

(** Lookup for variable-like identifier. Returns [None] if variable is not
  bound. *)
val lookup_var : t -> S.var -> (Module.var_info * on_use) option

(** Lookup for Unif representation, a scheme, and "on-use" function of a named
  implicit. Returns [None] if implicit is not bound. *)
val lookup_implicit :
  t -> S.iname -> (T.var * T.scheme * on_use) option
(*
(** Extend an environment with the label of a given scheme. *)
val add_the_label_sch : t -> T.scheme -> t * Var.t
*)
(** Lookup for implicit "label" variable. *)
val lookup_the_label : t -> (T.var * T.scheme * on_use) option
(*
(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT *)
val lookup_ctor : t -> S.ctor_name S.path -> (int * Module.adt_info) option
*)
(** Lookup for Unif representation of a type variable. Returns [None] if
  variable is not bound. *)
val lookup_tvar : t -> S.tvar -> (T.typ * on_use) option
(*
(** Lookup for a module of the given name. *)
val lookup_module : t -> S.module_name S.path -> Module.t option

(** Lookup for the effect variable labeled with "effect". It should always
  have an effect kind. *)
val lookup_the_effect : t -> T.typ option
*)
(** Lookup for ADT definition assigned for given type variable *)
val lookup_adt : t -> T.tvar -> Module.adt_info option

(** Lookup for method associated with given type variable *)
val lookup_method :
  t -> T.tvar -> S.method_name -> (T.var * T.scheme * on_use) option
(*
(** Lookup for pretty-printing information about type variable *)
val lookup_tvar_pp_info : t -> T.tvar -> pp_info option

(** Increase the level of the environment's scope. The level should be
  increased for each place, when implicit type generalization can occur. *)
val incr_level : t -> t
*)
(** Get the current scope *)
val scope : t -> T.scope
(*
(** Refresh all named parameters from a given scheme and extend the scope of
  the environment by adding all named parameters present in the refreshed
  scheme. *)
val extend_scope : t -> T.scheme -> t * T.scheme

(** Get a level of given environment *)
val level : t -> int
*)
(** Create a fresh unification variable in the current scope *)
val fresh_uvar : t -> T.kind -> T.typ

(** Create a new module on top of the module stack. *)
val enter_module : t -> t

(** Finalize a module definition and add it to the outer module with the
  given name. *)
val leave_module : t -> public:bool -> S.module_name -> t
(*
(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : t -> public:bool -> Module.t -> t
*)
