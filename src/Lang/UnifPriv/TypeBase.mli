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
  | TNEffect
  | TNVar of string

type named_tvar = tname * tvar

type name =
  | NLabel
  | NVar      of string
  | NImplicit of string
  | NMethod   of string

type typ

type effect = typ
type effrow = typ

type effrow_end =
  | EEClosed
  | EEUVar of TVar.Perm.t * uvar
  | EEVar  of tvar
  | EEApp  of typ * typ

type type_view =
  | TUVar      of TVar.Perm.t * uvar
  | TVar       of tvar
  | TEffect    of TVar.Set.t
  | TEffrow    of TVar.Set.t * effrow_end
  | TPureArrow of scheme * typ
  | TArrow     of scheme * typ * effrow
  | THandler   of tvar * typ * typ * effrow * typ * effrow
  | TLabel     of effect * typ * effrow
  | TApp       of typ * typ

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

(** Unification variable *)
val t_uvar : TVar.Perm.t -> uvar -> typ

(** Regular type variable *)
val t_var : tvar -> typ

(** Pure arrow type *)
val t_pure_arrow : scheme -> typ -> typ

(** Arrow type *)
val t_arrow : scheme -> typ -> effrow -> typ

(** Type of first-class handlers *)
val t_handler : tvar -> typ -> typ -> effrow -> typ -> effrow -> typ

(** Type of first-class label *)
val t_label : effect -> typ -> effrow -> typ

(** Create an effect *)
val t_effect : TVar.Set.t -> effect

(** Create an effect row *)
val t_effrow : TVar.Set.t -> effrow_end -> effrow

(** Create a closed effect row *)
val t_closed_effrow : TVar.Set.t -> effrow

(** Type application *)
val t_app : typ -> typ -> typ

(** Reveal a top-most constructor of a type *)
val view : typ -> type_view

(** Reveal a representation of an effect: a set of effect variables *)
val effect_view : effect -> TVar.Set.t

(** Reveal a representation of an effect row: a set of effect variables
  together with a way of closing an effect row. *)
val effrow_view : effrow -> TVar.Set.t * effrow_end

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
