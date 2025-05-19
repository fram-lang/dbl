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

let cannot_find_module pos path =
  (Some pos, Printf.sprintf "Cannot find module `%s'" path)

let module_dependency_cycle path =
  (None, Printf.sprintf "Module dependency cycle detected for `%s'" path)

let unexpected_token pos tok =
  (Some pos, Printf.sprintf "Unexpected token `%s'" tok)

let invalid_character pos ch =
  (Some pos, Printf.sprintf "Invalid character `%s'" (Char.escaped ch))

let eof_in_comment pos name =
  (Some pos,
    Printf.sprintf
      "Unexpected end of file inside a block comment (`%s#}' was expected)"
      name)

let invalid_number pos str =
  (Some pos, Printf.sprintf "Invalid integer literal `%s'" str)

let number_out_of_bounds pos str =
  (Some pos,
    Printf.sprintf "Integer literal %s exceeds the representable range" str)

let invalid_escape_code pos =
  (Some pos, "Invalid escape code")

let eof_in_string pos =
  (Some pos, "Unexpected end of file inside a string literal")

let invalid_lexer_directive ?msg pos =
  (Some pos,
    Printf.sprintf
      "Invalid lexer directive%s"
        (match msg with
        | None -> ""
        | Some msg -> ": " ^ msg))

let desugar_error pos =
  (Some pos, "Syntax error. This construction cannot be used in this context")

let reserved_binop_error pos op =
  (Some pos,
    Printf.sprintf
      "Syntax error. Operator %s can only be used in binary expressions"
      op)

let disallowed_op_error pos op =
  (Some pos,
    Printf.sprintf
      "Syntax error. Operator %s is disallowed to avoid ambiguity"
      op)

let invalid_pattern_arg pos =
  (Some pos,
  "Syntax error. This argument is provided to a pattern that do not expect it")

let impure_scheme pos =
  (Some pos, "Syntax error. Type schemes must be pure")

let type_alias_with_args pos =
  (Some pos, "Type aliases cannot have type arguments")

let finally_before_return_clause pos =
  (Some pos, "Finally clause before return clause")

let multiple_self_parameters pos =
  (Some pos, "Multiple 'self' parameters of a method")

let abstr_data_in_pub_block pos =
  (Some pos, "This 'abstr' data modifier has no effect. \
  It is overridden by 'public' of entire group of definitions.")

let existential_type_arg_in_record pos =
  (Some pos, "Existential type arguments are not allowed in record types")

let ignored_field_in_record pos =
  (Some pos, "This construct is ignored in records")
