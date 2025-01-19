(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Represention of module definitions *)

open Common

type 'st t

(** The top-level state of the module (before closing) *)
type top = (closed, modl) opn

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

(** Information about type *)
type type_info =
  | TI_Type of T.typ
    (** The type is defined and has given Unif representation *)

  | TI_Parameter of T.tvar
    (** Declared type parameter. Extra lookup is needed to get the actual
      type *)

(** Information about value identifier (variable, constructor, etc.) *)
type val_info =
  | VI_Var  of T.var * T.scheme
    (** Variable: its Unif representation and its type scheme *)

  | VI_Ctor of int * adt_info
    (** Constructor: its index and full information about ADT *)

  | VI_Parameter of UID.t
    (** Declared parameter. Extra lookup is needed to get the actual value *)

(** Empty module *)
val empty : top t

(** Extend the module with a named type variable. *)
val add_type_alias : public:bool ->
  ('st, 'sc) opn t -> S.tvar -> T.typ -> ('st, 'sc) opn t

(** Extend the module with a polymorphic valaue of given name *)
val add_val : public:bool ->
  ('st, 'sc) opn t -> Name.t -> T.var -> T.scheme -> ('st, 'sc) opn t

(** Assign ADT definition to given type variable. For abstract datatype,
  the [public] flag should be set to [false]. *)
val add_adt : public:bool ->
  ('st, 'sc) opn t -> T.tvar -> adt_info -> ('st, 'sc) opn t

(** Add constructor of given name and index to the module. *)
val add_ctor : public:bool ->
  ('st, 'sc) opn t -> S.ctor_name -> int -> adt_info -> ('st, 'sc) opn t

(** Extend the module with the definition of a module with the given name. *)
val add_module : public:bool ->
  ('st, 'sc) opn t -> S.module_name -> closed t -> ('st, 'sc) opn t

(** Extend the module with the declaration of a type *)
val declare_type :
  ('st, sec) opn t -> S.tvar -> T.kind -> ('st, sec) opn t * T.tvar

(** Extend the module with the declaration of a value parameter *)
val declare_val :
  ('st, sec) opn t -> Name.t -> ('st, sec) opn t * UID.t

(** Introduce the given module's identifiers into scope with visibility
  specified by [~public]. *)
val open_module : public:bool ->
  ('st, 'sc) opn t -> closed t -> ('st, 'sc) opn t

(** Finalize module definition: remove all private members and set
  pretty-printing information. *)
val leave : top t -> PPTree.pp_module -> closed t

(** Enter the new section scope. *)
val enter_section : ('st, 'sc) opn t -> (('st, 'sc) opn, sec) opn t

(** Leave the current section scope. *)
val leave_section : (('st, 'sc) opn, sec) opn t -> ('st, 'sc) opn t

(** Lookup for Unif representation of a type variable. Returns [None] if
  variable is not bound. *)
val lookup_tvar : 'st t -> S.tvar -> type_info option

(** Lookup for a value. Returns [None] if variable is not bound. *)
val lookup_val : 'st t -> Name.t -> val_info option

(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT. *)
val lookup_ctor : 'st t -> S.ctor_name -> (int * adt_info) option

(** Lookup for ADT definition assigned for given type variable *)
val lookup_adt : 'st t -> T.tvar -> adt_info option

(** Lookup for a module of the given name. *)
val lookup_module : 'st t -> S.module_name -> closed t option

(** Get pretty-printing information of the module. Can be called only on
  modules obtained by [leave]. *)
val pp_module : closed t -> PPTree.pp_module
