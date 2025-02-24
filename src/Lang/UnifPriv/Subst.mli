(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

open TypeBase

type t

(** Empty substitution. The [scope] argument should be the scope of the types
  that will be substituted. *)
val empty : scope:Scope.t -> t

(** Enter a new scope. *)
val enter_scope : t -> t

(** Extend substitution with a renaming, where the new version of a variable
  is generated automatically. *)
val add_tvar : t -> tvar -> t * tvar

(** Extend the substitution with a permutation that maps given type variables
  to their fresh version. It returns extended substitution together with
  refreshed type variables *)
val add_tvars : t -> tvar list -> t * tvar list

(** Same as [add_tvars], but works on named type variables *)
val add_named_tvars : t -> named_tvar list -> t * named_tvar list

(** Extend substitution *)
val add_type : t -> tvar -> typ -> t

(** Extend substitution with a renaming of type variable. Equivalent to
  [add_type sub x (Type.t_var y)] *)
val rename_tvar : t -> tvar -> tvar -> t

(** Substitute in type *)
val in_type : t -> typ -> typ

(** Substitute in a type scheme *)
val in_scheme : t -> scheme -> scheme

(** Substitute in a named type scheme *)
val in_named_scheme : t -> named_scheme -> named_scheme

(** Substitute in constructor definition *)
val in_ctor_decl : t -> ctor_decl -> ctor_decl
