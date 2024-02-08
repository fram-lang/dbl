(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors that may occur during the translation from Unif to Core *)

(* Author: Piotr Polesiuk, 2023,2024 *)

type t = Position.t * string

let fatal (pos, msg) =
  InterpLib.Error.report ~pos ~cls:FatalError msg;
  raise InterpLib.Error.Fatal_error

let non_exhaustive_match ~pos ctx =
  (* TODO: counterexample in the message. *)
  let msg = Printf.sprintf
    "This pattern-matching is not exhaustive."
  in (pos, msg)
