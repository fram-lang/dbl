(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking subtyping of types *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Arrow type *)
type arrow =
  | Arr_No
    (** Type is not an arrow *)

  | Arr_Pure of T.typ * T.typ
    (** Pure arrow *)

  | Arr_Impure of T.typ * T.typ * T.effect
    (** Impure arrow *)

(** Check if one effect is a subeffect of another.
  It performs some unifications when necessary. *)
val subeffect : Env.t -> T.effect -> T.effect -> bool

(** Check if one type is a subtype of another.
  It performs some unifications when necessary. *)
val subtype : Env.t -> T.typ -> T.typ -> bool

(** Coerce given type to an arrow.
  It performs some unifications when necessary. *)
val to_arrow : Env.t -> T.typ -> arrow

(** Coerce given type from an arrow.
  It performs some unifications when necessary. *)
val from_arrow : Env.t -> T.typ -> arrow 
