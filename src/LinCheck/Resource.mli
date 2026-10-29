(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Types used by the linearity checking. *)

(** Mode of a resource. *)
type mode =
  | Affine
    (** The resource can be used at most once. *)

(** Resource identifier. *)
type resource_id

(** Position where a resource is used. It is a sequence of positions where
  a resourceful function was called. The first position is the outermost call,
  and the last position is the call of the original resourceful function. *)
type use_pos = Position.t list

(** Resources used by a computation. *)
type usage =
  | RNone
    (** No resources are used. *)

  | RSome of resource_id * use_pos
    (** A resource is used at most once. *)

(** Approximation of the expression type. *)
type typ =
  | TAny
    (** Any type that do not mention resources. *)

  | TArrow of typ * usage
    (** Function type with the given return type and resource usage. *)

(** Create a fresh resource identifier. *)
val fresh : pos:Position.t -> mode -> resource_id

(** Create a function type with the given return type and resource usage.
  It is a smart constructor that simplifies the type if the usage is none
  and the return type is [TAny]. *)
val arrow : typ -> usage -> typ

(** Extract the return type and resource usage from a function type.
  The position is added to the resource usage in order to track where the
  resource came from. *)
val as_arrow : pos:Position.t -> typ -> typ * usage

(** Check if a resource usage is none. *)
val is_none : usage -> bool

(** Sequential join two resource usages. *)
val seq_join : usage -> usage -> usage

(** Parallel join two resource usages. *)
val par_join : usage -> usage -> usage

(** Union of two types. *)
val join_types : typ -> typ -> typ

(** Ensures that the type of the expression does not mention any resources.
  Raises an error if it does. *)
val ensure_no_resources_t : 'a Lang.Unif.node -> typ -> unit

(** Ensures that the resource usage of the expression is none.
  Raises an error if it is not. *)
val ensure_no_resources_r : 'a Lang.Unif.node -> usage -> unit

(** Ensures that the resource usage of the expression is compatible with the
  given resource identifier. Raises an error if it is not. *)
val check_usage : 'a Lang.Unif.node -> resource_id -> usage -> unit

(** Guard an expression to ensure that its effect is compatible with the
  given resource usage (of the computation that is executed after the
  expression). *)
val guard : Lang.Unif.expr -> usage -> Lang.Unif.expr
