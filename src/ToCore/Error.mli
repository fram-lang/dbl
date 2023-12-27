(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors that may occur during the translation from Unif to Core *)

(* Author: Piotr Polesiuk, 2023 *)

(** Abstract representation of error *)
type t

(** Report fatal error and abort the compilation *)
val fatal : t -> 'a

val non_exhaustive_match : pos:Position.t -> PatternContext.ctx -> t
