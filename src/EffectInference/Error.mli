(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to effect-inference *)

open Common

(** Abstract representation of errors *)
type t

(** Report fatal error and abort the compilation *)
val fatal : t -> 'a

(** Report a warning. *)
val warn : t -> unit

val escaping_effect_var : origin:origin -> T.tvar -> t

val non_exhaustive_match : pos:Position.t -> PatternContext.ctx -> t

val unused_pattern : pos:Position.t -> t

val unsolved_unification_variable : pos:Position.t -> t
