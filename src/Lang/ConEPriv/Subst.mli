(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Substitutions *)

open TypeBase

type t

(** Empty substitution *)
val empty : t

(** Add a renaming to the substitution. *)
val rename : t -> tvar -> tvar -> t

(** Add a type to the substitution. *)
val add : t -> tvar -> typ -> t

(** Create a new substitution that maps given type variables to the
  corresponding types. Both list must have equal lengths. *)
val for_named_tvars : named_tvar list -> typ list -> t

(** Apply the substitution to an effect *)
val in_effect : t -> effct -> effct

(** Apply the substitution to a type *)
val in_type : t -> typ -> typ

(** Apply the substitution to a type scheme *)
val in_scheme : t -> scheme -> scheme

(** Apply the substitution to a constructor declaration *)
val in_ctor_decl : t -> ctor_decl -> ctor_decl
