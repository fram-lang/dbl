(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The environment of the linearity constraint inference *)

type t

(** Initial environment *)
val initial : t

(** Add a variable with a given type to the environment *)
val add_var : t -> Var.t -> Resource.typ -> t

(** Add a list of variables to the environment. All variables will have
  [TAny] type. *)
val add_vars : t -> Var.t list -> t

(** Enter a new recursive context. In the recursive context it is possible to
  add recursive variables [add_rec_vars], but they become visible only after
  the call to [commit_rec_context]. The recursive context should not be
  nested. *)
val enter_rec_context : t -> t

(** Commit the recursive context. The recursive variables become visible in the
  environment. *)
val commit_rec_context : t -> t

(** Add recursive variables to the environment. They will be visible only after
  the call to [commit_rec_context]. *)
val add_rec_vars : t -> Var.t list -> t

(** Lookup the type of a variable in the environment *)
val lookup_var : t -> Var.t -> Resource.typ
