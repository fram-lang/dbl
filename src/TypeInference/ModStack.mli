(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Stack of modules which are currently being defined *)

open Common

type t

(** The top-level module stack *)
val toplevel : t

(** Extend the current module with a polymorphic variable *)
val add_var : t -> public:bool -> S.var -> T.scheme -> t * T.var

(** Extend the current module with a named type variable. *)
val add_tvar : t -> public:bool -> S.tvar -> T.kind -> t * T.tvar

(** Extend the current module with a type alias. *)
val add_type_alias : t -> public:bool -> S.tvar -> T.typ -> t

(** Extend the current module with a polymorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_implicit :
  t -> public:bool -> S.iname -> T.scheme -> (Position.t -> unit) -> t * T.var

(** Add constructor of given name and index to the current module. *)
val add_ctor : t -> public:bool -> string -> int -> Module.adt_info -> t

(** Lookup for Unif representation and a scheme of a variable. Returns [None]
  if variable is not bound. *)
val lookup_var : t -> S.var S.path -> (T.var * T.scheme) option

(** Lookup for Unif representation, a scheme, and "on-use" function of a named
  implicit. Returns [None] if implicit is not bound. *)
val lookup_implicit :
  t -> S.var S.path -> (T.var * T.scheme * (Position.t -> unit)) option

(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT. *)
val lookup_ctor : t -> S.ctor_name S.path -> (int * Module.adt_info) option

(** Lookup for Unif representation of a type variable. Returns [None] if
  variable is not bound. *)
val lookup_tvar : t -> S.tvar S.path -> T.typ option

(** Lookup for a module of the given name. *)
val lookup_module : t -> S.module_name S.path -> Module.t option

(** Extend the given set of unification variables by unification variables
  found in the module stack. *)
val collect_uvars : t -> T.UVar.Set.t -> T.UVar.Set.t

(** Create a new module on top of the module stack *)
val enter_module : t -> t

(** Finalize a module definition and add it to the outer module with the
  given name. *)
val leave_module : t -> public:bool -> S.module_name -> t

(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : t -> public:bool -> Module.t -> t
