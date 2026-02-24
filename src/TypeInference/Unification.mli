(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unification and subtyping of types *)

open Common

(** Arrow type *)
type arrow =
  | Arr_No
    (** Type is not an arrow *)

  | Arr_UVar
    (** Type is a unification variable. It will never be returned by
      [to_arrow] *)

  | Arr_Arrow of T.scheme * T.typ * T.effct
    (** Effect-annotated arrow *)

(** Handler type *)
type handler =
  | H_No
    (** Type is not a handler *)

  | H_Handler of T.tvar * T.typ * T.typ * T.typ
    (** Handler type *)

(** Label type *)
type label =
  | L_No
    (** Type is not a label *)

  | L_Label of T.typ
    (** Label type. It stores the type of the delimiter. *)

(** Extra information that can be attached to error occurred during
  unification. *)
type error_info =
  | TVarEscapesScope of PPTree.t * T.tvar

(** Result of unification *)
type result =
  | Unify_Success
    (** Unification succeeded *)
  | Unify_Fail of error_info list
    (** Unification failed with list of errors *)

(** Check if one kind is equal to another. It performs some unifications
  when necessary. *)
val unify_kind : T.kind -> T.kind -> bool

(** Ensure that given kind is an arrow. It performs some unifications when
  necessary. On success, it returns LHS and RHS kinds of an arrow kind. *)
val kind_to_arrow : T.kind -> (T.kind * T.kind) option

(** Check if two types (of the same kind) are equivalent.
  It performs some unifications when necessary. *)
val unify_type : 'st Env.t -> T.typ -> T.typ -> result

(** Check if one type is a subtype of another.
  It performs some unifications when necessary. *)
val subtype : 'st Env.t -> T.typ -> T.typ -> result

(** Check if one scheme is a subscheme of another.
  It performs some unifications when necessary. *)
val subscheme : 'st Env.t -> T.scheme -> T.scheme -> result

(** Check if two schemes are equal.
  It performs some unifications when necessary. *)
val equal_scheme : 'st Env.t -> T.scheme -> T.scheme -> result

(** Coerce given type to an arrow.
  It performs some unifications when necessary. Never returns [Arr_UVar]. *)
val to_arrow : pos:Position.t -> 'st Env.t -> T.typ -> arrow

(** Coerce given type from an arrow.
  It performs some unifications when necessary. Returns [Arr_UVar] when given
  type is an unification variable. *)
val from_arrow : 'st Env.t -> T.typ -> arrow 

(** Coerce given type to a handler.
  It performs some unification when necessary. *)
val to_handler : pos:Position.t -> 'st Env.t -> T.typ -> handler

(** Coerce given type from a handler.
  It performs some unification when necessary. *)
val from_handler : pos:Position.t -> 'st Env.t -> T.typ -> handler

(** Coerce given type to a label type.
  It performs some unification when necessary. *)
val to_label : pos:Position.t -> 'st Env.t -> T.typ -> label
