(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unification and subtyping of types *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Arrow type *)
type arrow =
  | Arr_No
    (** Type is not an arrow *)

  | Arr_UVar
    (** Type is a unification variable. It will never be returned by
      [to_arrow] *)

  | Arr_Pure of T.scheme * T.typ
    (** Pure arrow *)

  | Arr_Impure of T.scheme * T.typ * T.effect
    (** Impure arrow *)

(** Handler type *)
type handler =
  | H_No
    (** Type is not a handler *)

  | H_Handler of T.tvar * T.typ * T.typ * T.effect
    (** Handler type *)

(** Check if one kind is equal to another. It performs some unifications
  when necessary. *)
val unify_kind : T.kind -> T.kind -> bool

(** Ensure that given kind is an arrow. It performs some unifications when
  necessary. On success, it returns LHS and RHS kinds of an arrow kind. *)
val kind_to_arrow : T.kind -> (T.kind * T.kind) option

(** Check if one effect is a subeffect of another.
  It performs some unifications when necessary. *)
val subeffect : Env.t -> T.effect -> T.effect -> bool

(** Check if one type is a subtype of another.
  It performs some unifications when necessary. *)
val subtype : Env.t -> T.typ -> T.typ -> bool

(** Check if one scheme is a subscheme of another.
  It performs some unifications when necessary. *)
val subscheme : Env.t -> T.scheme -> T.scheme -> bool

(** Coerce given type to an arrow.
  It performs some unifications when necessary. *)
val to_arrow : Env.t -> T.typ -> arrow

(** Coerce given type from an arrow.
  It performs some unifications when necessary. Returns [Arr_UVar], when given
  type is an unification variable. *)
val from_arrow : Env.t -> T.typ -> arrow 

(** Coerce given type to a handler.
  It performs some unification when necessary. *)
val to_handler : Env.t -> T.typ -> handler

(** Coerce given type from a handler.
  It performs some unification when necessary. *)
val from_handler : Env.t -> T.typ -> handler
