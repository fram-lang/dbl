(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Collecting constraints arising from subtyping. *)

open Common

(** Ensure that one computation effect is a subeffect of another *)
val subceffect : origin:origin -> Env.t -> T.ceffect -> T.ceffect -> unit

(** Ensure that one type is less then another *)
val subtype : origin:origin -> Env.t -> T.typ -> T.typ -> unit

(** Ensure that one type scheme is less then another *)
val subscheme : origin:origin -> Env.t -> T.scheme -> T.scheme -> unit

(** Create a type with the same shape as the given one, but with all effects
  replaced with fresh generalizable variables at the scope of the environment.
  The type might be later used with [subtype] function to ensure that type
  variables don't escapes their scopes. *)
val type_shape : Env.t -> T.typ -> T.typ

(** Decompose a type into components of an arrow type *)
val as_arrow : T.typ -> T.scheme * T.typ * T.ceffect

(** Decompose a type into components of a label type *)
val as_label : T.typ -> T.effct * T.typ * T.effct

(** Decompose a type into components of a handler type *)
val as_handler : T.typ -> T.tvar * T.typ * T.typ * T.effct * T.typ * T.effct
