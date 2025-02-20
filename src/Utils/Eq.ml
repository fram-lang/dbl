(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Equality of types, encoded using GADT. *)

type (_, _) t =
  | Equal    : ('a, 'a) t
  | NotEqual : ('a, 'b) t
