(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-checking of explicit instantiations *)

open Common
open TypeCheckFix

(** Instantiate polymorphic expression of given scheme by providing the
  explicit instantiation. *)
val instantiate_poly_expr : tcfix:tcfix ->
  pos:Position.t -> 'st Env.t -> T.poly_expr -> T.scheme -> S.inst list ->
    infer expr_result
