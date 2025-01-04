(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal implementation of types in the Unif Language. *)

open KindBase

type uvar
type tvar  = TVar.t
type scope = Scope.t

type tname =
  | TNAnon
  | TNVar of string

type named_tvar = tname * tvar

type name =
  | NVar         of string
  | NOptionalVar of string
  | NImplicit    of string
  | NMethod      of string

type effect = Pure | Impure

type typ

type type_view =
  | TEffect
  | TUVar    of TVar.Perm.t * uvar
  | TVar     of tvar
  | TArrow   of scheme * typ * effect
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
val t_uvar : TVar.Perm.t -> uvar -> typ

(** Regular type variable *)
val t_var : tvar -> typ

(** Arrow type *)
val t_arrow : scheme -> typ -> effect -> typ

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

  val fresh : scope:scope -> kind -> uvar

  (** Get the kind of given unification variable *)
  val kind : t -> kind

  val equal : t -> t -> bool

  val uid : t -> UID.t

  val scope : t -> scope

  val level : t -> int

  (** Set a unification variable, without checking any constraints. It returns
    expected scope of set type. The first parameter is a permutation attached
    to unification variable *)
  val raw_set : TVar.Perm.t -> t -> typ -> scope

  val fix : t -> tvar

  (** Shrink scope of given unification variable to given level, leaving only
    those variables which satisfy given predicate. *)
  val filter_scope : uvar -> int -> (tvar -> bool) -> unit

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end
