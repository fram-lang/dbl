(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to parsing *)

(* Author: Piotr Polesiuk, 2023,2024 *)

type t = unit

let fatal () =
  InterpLib.Error.incr_error_counter ();
  raise InterpLib.Error.Fatal_error

let cannot_read_file ?pos ~fname msg =
  (* TODO: not implemented properly *)
  Printf.eprintf "error: cannot read file %s (%s)\n" fname msg

let cannot_open_file ?pos ~fname msg =
  (* TODO: not implemented properly *)
  Printf.eprintf "error: cannot open file %s (%s)\n" fname msg

let unexpected_token pos tok =
  (* TODO: not implemented properly *)
  Printf.eprintf "%s: error: unexpected token `%s'\n"
    (Position.to_string pos) tok

let invalid_character pos ch =
  (* TODO: not implemented properly *)
  Printf.eprintf "%s: error: invalid character `%s'\n"
    (Position.to_string pos) (Char.escaped ch)

let eof_in_comment pos =
  (* TODO: not implemented properly *)
  Printf.eprintf "%s: error: unexpected end of file inside a block comment\n"
    (Position.to_string pos)

let desugar_error pos =
  (* TODO: not implemented properly *)
  Printf.eprintf
    "%s: error: syntax error. This construction cannot be used in this context.\n"
    (Position.to_string pos)

let invalid_pattern_arg pos =
  (* TODO: not implemented properly *)
  Printf.eprintf
    "%s: error: syntax error. This argument is provied to a pattern that do not expect it.\n"
    (Position.to_string pos)

let impure_scheme pos =
  (* TODO: not implemented properly *)
  Printf.eprintf
    "%s: error: syntax error. Type schemes must be pure.\n"
    (Position.to_string pos)
