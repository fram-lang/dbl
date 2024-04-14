(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to parsing *)

type t = Position.t option * string

let fatal (pos, msg) =
  InterpLib.Error.report ?pos ~cls:FatalError msg;
  raise InterpLib.Error.Fatal_error

let warn (pos, msg) =
  InterpLib.Error.report ?pos ~cls:Warning msg

let cannot_read_file ?pos ~fname msg =
  (pos, Printf.sprintf "Cannot read file %s (%s)" fname msg)

let cannot_open_file ?pos ~fname msg =
  (pos, Printf.sprintf "Cannot open file %s (%s)" fname msg)

let unexpected_token pos tok =
  (Some pos, Printf.sprintf "Unexpected token `%s'" tok)

let invalid_character pos ch =
  (Some pos, Printf.sprintf "Invalid character `%s'" (Char.escaped ch))

let eof_in_comment pos =
  (Some pos, "Unexpected end of file inside a block comment")

let invalid_number pos str =
  (Some pos, Printf.sprintf "Invalid integer literal `%s'" str)

let number_out_of_bounds pos str =
  (Some pos,
    Printf.sprintf "Integer literal %s exceeds the representable range" str)

let invalid_escape_code pos =
  (Some pos, "Invalid escape code")

let eof_in_string pos =
  (Some pos, "Unexpected end of file inside a string literal")

let desugar_error pos =
  (Some pos, "Syntax error. This construction cannot be used in this context")

let reserved_binop_error pos op =
  ( Some pos
  , "Syntax error. Operator " ^ op ^ " can only be used in binary expressions"
  )

let invalid_pattern_arg pos =
  (Some pos,
  "Syntax error. This argument is provided to a pattern that do not expect it")

let impure_scheme pos =
  (Some pos, "Syntax error. Type schemes must be pure")

let anon_type_pattern pos =
  (Some pos, "Syntax error. Anonymous types cannot be explicitly bound")

let value_before_type_param pos =
  (Some pos, "Named value parameter appears before a type parameter")

let finally_before_return_clause pos =
  (Some pos, "Finally clause before return clause")

let multiple_self_parameters pos =
  (Some pos, "Multiple 'self' parameters of a method")
