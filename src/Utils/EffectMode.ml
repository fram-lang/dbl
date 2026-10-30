(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Effect modes. *)

type t =
  | Unrestricted
    (** Unrestricted effect, i.e., no particular mode. *)

  | Affine
    (** Affine effect, i.e., the computation returns at most once. *)

(** Extended effect mode, which includes a flag indicating whether a resource
  usage should be checked. *)
type extended =
  { mode         : t;
    should_check : bool
  }
