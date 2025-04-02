(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating ConE to S-expressions *)

open TypeBase
open Syntax

val tr_type : typ -> SExpr.t

val tr_scheme : scheme -> SExpr.t

val tr_constr : constr -> SExpr.t

val tr_program : program -> SExpr.t
