(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module for reporting errors *)

(* Author: Piotr Polesiuk, 2023 *)

exception Fatal_error

let err_counter = ref 0

let incr_error_counter () =
  err_counter := !err_counter + 1

let assert_no_error () =
  if !err_counter <> 0 then
    raise Fatal_error
