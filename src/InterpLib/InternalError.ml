(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module for reporting internal errors errors *)

let verbose = ref false

let sexpr_info name s =
  match s with
  | None   -> ()
  | Some s ->
    Printf.eprintf "%s\n" name;
    SExpr.pretty_stderr s

let report ~reason ?sloc ?requested ?provided ?var ?in_type ?in_effect () =
  if !verbose then begin
    Printf.eprintf "Internal error: %s\n" reason;
    sexpr_info "at:"        sloc;
    sexpr_info "requested:" requested;
    sexpr_info "provided:"  provided;
    sexpr_info "var:"       var;
    sexpr_info "type:"      in_type;
    sexpr_info "effect:"    in_effect
  end;
  failwith (Printf.sprintf "Internal error: %s" reason)
