(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kind

type kind_view =
  | KType

(** Kind of all types *)
val k_type : kind

(** Reveal a top-most constructor of a kind *)
val view : kind -> kind_view
