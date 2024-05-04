(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

let usage_string =
  Printf.sprintf
    "Usage: %s [OPTION]... FILE [CMD_ARG]...\nAvailable OPTIONs are:"
    Sys.argv.(0)

let args = ref []

let cmd_args_options = Arg.align
  [ "-args",
    Arg.Rest (fun arg -> args := arg :: !args),
    "[CMD_ARG]... Pass remaining arguments to the interpreted program";

    "-dcore",
    Arg.Set Pipeline.dump_core,
    " Dump internal Core representation";

    "-no-prelude",
    Arg.Clear Pipeline.use_prelude,
    " Disable the prelude";

    "-no-stdlib",
    Arg.Clear Pipeline.use_stdlib,
    " Do not use the standard library";

    "-verbose-internal-errors",
    Arg.Set InterpLib.InternalError.verbose,
    " Make internal errors more verbose (for debugging only)"
  ]

let fname = ref None

let proc_arg arg =
  match !fname with
  | None   ->
    fname := Some arg;
    (* Hack: change current argument to -args and reparse it in order
      to pass remaining arguments to the interpreted program *)
    Sys.argv.(!Arg.current) <- "-args";
    Arg.current := !Arg.current - 1
  | Some _ -> assert false

let _ =
  Arg.parse cmd_args_options proc_arg usage_string;
  try
    match !fname with
    | None       -> Pipeline.run_repl ()
    | Some fname -> Pipeline.run_file fname
  with
  | InterpLib.Error.Fatal_error -> exit 1
