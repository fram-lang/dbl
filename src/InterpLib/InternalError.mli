(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module for reporting internal errors errors *)

(** Flag that indicate that internal error should be more verbose *)
val verbose : bool ref

(** Report an internal error. The meaning of the parameters is the following.
  - [reason]:    short readable reason of the error, e.g., type mismatch;
  - [sloc]:      s-expression with the location of the error, e.g., the invalid
                 expression;
  - [requested]: requested metadata of the erroneous entity, e.g., its
                 requested type;
  - [provided]:  provided metadata of the erroneous entity, e.g., its actual
                 type.
  - [var]:       variable associated with the error (e.g., escaping variable)
  - [in_type]:   type of an erroneous entity.
  - [in_effect]: effect of an erroneous entity. *)
val report :
  reason:     string ->
  ?sloc:      SExpr.t ->
  ?requested: SExpr.t ->
  ?provided:  SExpr.t ->
  ?var:       SExpr.t ->
  ?in_type:   SExpr.t ->
  ?in_effect: SExpr.t ->
  unit -> 'a
