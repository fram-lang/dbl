(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kind

type kind_view =
  | KType
  | KEffrow

(** Kind of all types *)
val k_type : kind

(** Kind of all effect rows *)
val k_effrow : kind

(** Reveal a top-most constructor of a kind *)
val view : kind -> kind_view
