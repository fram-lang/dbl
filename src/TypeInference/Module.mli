(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Represention of module definitions *)

open Common

type t

(** Type of function that is called on each use of the variable *)
type on_use = Position.t -> unit

(** Information about an ADT definition *)
type adt_info = {
  adt_proof  : T.poly_expr;
    (** A computationally irrelevant expression that give a proof that given
      type is an ADT. It is polymorphic in the type parameters of an ADT. *)

  adt_args   : T.named_tvar list;
    (** Type parameter of an ADT *)

  adt_ctors  : T.ctor_decl list;
    (** List of constructors of an ADT *)

  adt_type   : T.typ;
    (** The type that is an ADT, already applied to [adt_args] *)

  adt_effect : T.effect
    (** An effect of pattern-matching on this type. Generally it is [Pure]
      for strictly poisitively recursive types. *)
}

(** Information about variable-like identifier (variable, constructor, etc.) *)
type var_info =
  | VI_Var  of T.var * T.scheme
    (** Variable: its Unif representation and its type scheme *)

  | VI_Ctor of int * adt_info
    (** Constructor: its index and full information about ADT *)

  | VI_MethodFn of S.method_name
    (** Function that is automatically translated to method call *)
(*
(** The built-in unit type *)
val unit_info : adt_info

(** Empty module *)
val empty : t
*)
(** The top-level module containg the base types *)
val toplevel : t
(*
(** Extend the module with a polymorphic variable *)
val add_var : t -> public:bool -> S.var -> T.scheme -> t * T.var

(** Extend the module with information that given identifier when used
  as function is a method of given name. *)
val add_method_fn : t -> public:bool -> S.var -> S.method_name -> t

(** Extend the module with a named type variable. *)
val add_tvar : t -> public:bool -> S.tvar -> T.kind -> t * T.tvar

(** Extend the module with a type alias. *)
val add_type_alias : t -> public:bool -> S.tvar -> T.typ -> t

(** Extend the module with a polymorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_implicit :
  t -> public:bool -> S.iname -> T.scheme -> (Position.t -> unit) -> t * T.var

(** Add constructor of given name and index to the module. *)
val add_ctor : t -> public:bool -> string -> int -> adt_info -> t

(** Extend the module with the definition of a module with the given name. *)
val add_module : t -> public:bool -> S.module_name -> t -> t
*)
(** Lookup for variable-like identifier. Returns [None] if variable is not
  bound. *)
val lookup_var : t -> S.var -> (var_info * on_use) option

(** Lookup for Unif representation, a scheme, and "on-use" function of a named
  implicit. Returns [None] if implicit is not bound. *)
val lookup_implicit :
  t -> S.iname -> (T.var * T.scheme * on_use) option

(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT. *)
val lookup_ctor : t -> S.ctor_name -> (int * adt_info) option

(** Lookup for ADT definition assigned for given type variable *)
val lookup_adt : t -> T.tvar -> adt_info option

(** Lookup for Unif representation of a type variable. Returns [None] if
  variable is not bound. *)
val lookup_tvar : t -> S.tvar -> (T.typ * on_use) option

(** Lookup for a module of the given name. *)
val lookup_module : t -> S.module_name -> t option
(*
(** Use the given lookup function on a module at the specified path.
  Returns [None] if the path is invalid or the lookup fails. *)
val lookup_path : t -> (t -> 'a -> 'b option) -> 'a S.path -> 'b option

(** Remove all private definitions from the module. *)
val filter_public : t -> t

(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : t -> public:bool -> t -> t
*)

(** Get the list of public type names *)
val public_types : t -> S.tvar list

(** Get the list of public variables *)
val public_vars : t -> S.var list

(** Get the list of public implicit variables *)
val public_implicits : t -> S.iname list
