(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Additional environment used in type-checking definition blocks. It stores
  information about declared named implicits. *)

open Common

type t

(** List of implicit that can be generalized. It can be created by
  [begin_generalize] function and consumed by [end_generalize] *)
type implicit_list

(** Empty environment *)
val empty : t

(** Prepare environment to generalizing implicit parameters: it adds implicit
  parameters to the environment and increases the level of the environment. *)
val begin_generalize : Env.t -> t -> Env.t * implicit_list

(** Build a list of types and implicit parameters that should be implicitly
  generalized. The second parameter is a set of unification variables that
  appears in the type/scheme of generalized entity. This function tries to
  generalize only those implicits that were used. *)
val end_generalize_pure :
  implicit_list -> T.UVar.Set.t ->
  T.named_tvar list * (T.name * T.var * T.scheme) list

(** Ensure, that no implicits on a given list were used and the given type
  fits in the scope of [begin_generalize] that created the implicit list.
  The position and environment are used to report escaping type variables,
  so the environment should be as wide as the given type. *)
val end_generalize_impure :
  pos:Position.t -> env:Env.t -> implicit_list -> T.typ -> unit

(** Extend environment with a declaration of implicit *)
val declare_implicit : t -> S.iname -> T.named_tvar list -> T.scheme -> t

(** Shadow existing implicit name *)
val shadow : t -> S.iname -> t

(** Shadow all implicit names bound in given man *)
val shadow_names : t -> 'a T.Name.Map.t -> t

(** Extend environment with a polymorphic identifier. If it is a name, then
  it will be shadowed in implicit environment. *)
val add_poly_id :
  pos:Position.t -> Env.t -> t -> S.ident -> T.scheme -> Env.t * t * T.var
