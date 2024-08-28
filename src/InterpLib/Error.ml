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

let repl_input = Buffer.create 512

let incr_error_counter () =
  err_counter := !err_counter + 1

let color_printer_generator color s =
  if not !DblConfig.display_colors
  then s
  else TextRangePrinting.color_string color s

let report ?pos ~cls msg =
  let module Color = TextRangePrinting in
  let name, color =
    match cls with
    | FatalError ->
      incr_error_counter ();
      "fatal error", Color.Red
    | Error ->
      incr_error_counter ();
      "error", Color.Red
    | Warning -> "warning", Color.Yellow
    | Note    -> "note", Color.Teal
  in
  let name = Color.color_string color name in
  let text_range = Option.bind pos
    (TextRangePrinting.get_text_range
      ~repl_input:(Buffer.contents repl_input)
      ~color) in
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
