(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of names of named parameters *)

(* Author: Piotr Polesiuk, 2024 *)

open Common

val tr_name : Env.t -> S.name -> T.name
