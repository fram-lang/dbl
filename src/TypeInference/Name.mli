(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of names of named parameters *)

open Common

val tr_tname : S.tname -> T.tname

val tr_name : Env.t -> S.name -> T.name
