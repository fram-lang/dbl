(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors that may occur during the translation from Unif to Core *)

(* Author: Piotr Polesiuk, 2023 *)

type t = unit

let fatal () =
  InterpLib.Error.incr_error_counter ();
  raise InterpLib.Error.Fatal_error

let non_exhaustive_match ~pos ctx =
  (* TODO: better message *)
  Printf.eprintf "%s: error: This pattern-matching is not exhaustive\n"
    (Position.to_string pos)
