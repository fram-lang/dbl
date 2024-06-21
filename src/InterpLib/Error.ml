(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module for reporting errors *)

exception Fatal_error

type error_class =
  | FatalError
  | Error
  | Warning
  | Note

let err_counter = ref 0

let repl_input = ref ""

let incr_error_counter () =
  err_counter := !err_counter + 1

let color_printer_generator color s =
  if not !DblConfig.display_colors
  then s
  else if color = "red" then
    Spectrum.Simple.sprintf ("@{<red>@{<bold>%s@}@}") s
  else if color = "yellow" then
    Spectrum.Simple.sprintf ("@{<yellow>@{<bold>%s@}@}") s
  else if color = "teal" then
    Spectrum.Simple.sprintf ("@{<teal>@{<bold>%s@}@}") s
  else raise (Invalid_argument "ARG")


let report ?pos ~cls msg =
  let name, color =
    match cls with
    | FatalError ->
      incr_error_counter ();
      "fatal error", "red"
    | Error ->
      incr_error_counter ();
      "error", "red"
    | Warning -> "warning", "yellow"
    | Note    -> "note", "teal"
  in
  let color_printer s = color_printer_generator color s in
  let name = color_printer name in
  let text_range = Option.bind pos
    (TextRangePrinting.get_text_range
      ~repl_input:!repl_input
      ~color_printer) in
  match pos, text_range with
  | Some pos, None ->
    Printf.eprintf "%s: %s: %s\n"
      (Position.to_string pos) name msg
  | _, Some pos ->
    Printf.eprintf "%s: %s\n%s\n"
      name msg pos
  | None, _ ->
    Printf.eprintf "%s: %s\n" name msg

let assert_no_error () =
  if !err_counter <> 0 then
    raise Fatal_error

let wrap_repl_cont cont () =
  let v = cont () in
  assert_no_error ();
  v

let reset () =
  err_counter := 0
