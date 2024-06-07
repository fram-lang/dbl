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

let incr_error_counter () =
  err_counter := !err_counter + 1

let report_to_stderr ?pos ~cls msg =
  let name =
    match cls with
    | FatalError ->
      incr_error_counter ();
      "fatal error"
    | Error ->
      incr_error_counter ();
      "error"
    | Warning -> "warning"
    | Note    -> "note"
  in
  match pos with
  | None ->
    Printf.eprintf "%s: %s\n" name msg
  | Some pos ->
    Printf.eprintf "%s: %s: %s\n"
      (Position.to_string pos) name msg

let report_impl = ref report_to_stderr

let set_report_function f = report_impl := f

let report ?pos ~cls msg = !report_impl ?pos ~cls msg

let assert_no_error () =
  if !err_counter <> 0 then
    raise Fatal_error

let wrap_repl_cont cont () =
  let v = cont () in
  assert_no_error ();
  v

let reset () =
  err_counter := 0
