(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Part of the environment that manages section parameters. *)

open Common

type t

(** List of parameters that can be implicitly generalized. It can be created
  by [begin_generalize] function and consumed by [end_generalize] functions.
  *)
type param_list

(** Status of a type parameter. *)
type ('name, 'a) param_status =
  | StNotGeneralized of 'name * Position.t
    (** Parameter is not generalized -- we didn't enter any scope when it
      could be generalized. The constructor stores its name and the position
      of the declaration. *)

  | StValid of 'a
    (** Parameter is valid -- it was generalized and can be used. The constructor
      stores the variable and scheme assigned to this parameter. *)

  | StRejected of 'name * Position.t
    (** Parameter is rejected -- it was not used at the time of generalization,
      but it is still visible in the environment, for instance in unsolved
      constraints. *)

(** Place of use of the parameter (for error reporting) *)
type 'name use =
  { u_name     : 'name;
    (** The name of the parameter *)

    u_use_pos  : Position.t;
    (** The place where the parameter was used *)

    u_decl_pos : Position.t
    (** The place where the parameter was declared *)
  }

(** Empty parameter environment. *)
val empty : t

(** Get the scope at the place of generalization. *)
val scope : param_list -> Scope.t

(** Get the pretty-printing information of extended environment *)
val pp_tree : param_list -> PPTree.t

(** Prepare environment for generalizing named parameters: move all parameters
  from "not generalized" state to the "generalized, but not used" state.
  This function modifies the scope of the environment (enters the new
  level). *)
val begin_generalize :
  pp:PPTree.t -> t -> Scope.t -> t * Scope.t * param_list

(** Build a list of types and named parameters that should be implicitly
  generalized. This function tries to generalize only those parameters that
  were used. *)
val end_generalize_pure :
  param_list ->
    (T.tname * UID.t * T.tvar) list * (Name.t * T.var * T.scheme_expr) list

(** Ensure that no named parameters on a given list were used. After calling
  this function, given named parameter become unavailable. Returns lists of
  parameters that were used. *)
val end_generalize_impure : param_list -> T.tname use list * Name.t use list

(** Add a type declaration to the environment. *)
val declare_type : pos:Position.t -> t -> T.tname -> UID.t -> T.kind -> t

(** Extend environment with a declaration of a value parameter. The meaning of
  parameters is the following.
  - [free_types] -- type variables that occur freely in the scheme of this
    parameter and should be separately instantiated for each use.
  - [used_types] -- previously declared type UIDs, together with the type
    variables used as them in the scheme of this parameter.
  - [name] -- the name of the parameter that will be visible in the
    generalized scheme. *)
val declare_val : pos:Position.t ->
  t -> free_types:T.tvar list -> used_types:(UID.t * T.tvar) list ->
    name:Name.t -> UID.t -> T.scheme_expr -> t

(** Try to access type parameter and return its status. *)
val check_type_param :
  pos:Position.t -> t -> UID.t -> (T.tname, T.tvar) param_status

(** Try to access value parameter and return its status. *)
val check_val_param :
  pos:Position.t -> t -> UID.t -> (Name.t, T.var * T.scheme) param_status
