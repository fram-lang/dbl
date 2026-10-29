(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to linearity *)

type mode =
  | Affine

(** Abstract representation of errors *)
type t

(** Report non-fatal error *)
val report : t -> unit

val resource_used_twice :
  def_at:Position.t -> mode:mode -> Position.t list -> Position.t list -> t

val two_resources_used :
  def1_at:Position.t -> def2_at:Position.t ->
    Position.t list -> Position.t list -> t

val resource_leaks :
  Position.t -> def_at:Position.t -> mode:mode -> Position.t list -> t

val invalid_resource_use :
  Position.t -> def_at:Position.t -> mode:mode -> Position.t list -> t
