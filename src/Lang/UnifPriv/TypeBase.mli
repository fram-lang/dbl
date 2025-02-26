(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

open UnifCommon

type kind = Kind.t

type uvar
type tvar = TVar.t

type tname = Names.tname =
  | TNAnon
  | TNVar of string

type named_tvar = tname * tvar

type name = Names.name =
  | NVar         of string
  | NOptionalVar of string
  | NImplicit    of string
  | NMethod      of string

type effct = Pure | Impure

type typ

type type_view =
  | TEffect
  | TUVar    of uvar
  | TVar     of tvar
  | TArrow   of scheme * typ * effct
  | THandler of tvar * typ * typ * typ
  | TLabel   of typ
  | TApp     of typ * typ

and scheme = {
  sch_targs : named_tvar list;
  sch_named : named_scheme list;
  sch_body  : typ
}

and named_scheme = name * scheme

type ctor_decl = {
  ctor_name        : string;
  ctor_targs       : named_tvar list;
  ctor_named       : named_scheme list;
  ctor_arg_schemes : scheme list
}

(** Effect *)
val t_effect : typ

(** Unification variable *)
val t_uvar : uvar -> typ

(** Regular type variable *)
val t_var : tvar -> typ

(** Arrow type *)
val t_arrow : scheme -> typ -> effct -> typ

(** Type of first-class handlers *)
val t_handler : tvar -> typ -> typ -> typ -> typ

(** Type of first-class label *)
val t_label : typ -> typ

(** Type application *)
val t_app : typ -> typ -> typ

(** Reveal a top-most constructor of a type *)
val view : typ -> type_view

(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  val fresh : scope:Scope.t -> kind -> uvar

  (** Get the kind of given unification variable *)
  val kind : t -> kind

  val equal : t -> t -> bool

  val uid : t -> UID.t

  val scope : t -> Scope.t

  (** Set a unification variable, without checking any constraints. It returns
    expected scope of set type. *) 
  val raw_set : t -> typ -> Scope.t

  val fix : t -> tvar

  (** Update the scope of a unification variable. Its scope is set to the
    intersection of its current scope and the given scope. *)
  val shrink_scope : t -> Scope.t -> unit

  val in_scope : t -> Scope.t -> bool

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end
