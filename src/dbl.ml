(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

let usage_string =
  Printf.sprintf
    "Usage: %s [OPTION]... FILE [CMD_ARG]...\nAvailable OPTIONs are:"
    Sys.argv.(0)

let cli_lib_search_dirs = ref []
let cli_local_search_dirs = ref []

let include_cli_search_dirs () = 
  DblConfig.lib_search_dirs :=
    List.rev_append !cli_lib_search_dirs !DblConfig.lib_search_dirs;
  DblConfig.local_search_dirs :=
    List.rev_append !cli_local_search_dirs !DblConfig.local_search_dirs

let cmd_args_options = Arg.align
  [ "-args",
    Arg.Rest_all (fun args -> DblConfig.prog_args := args),
    "[CMD_ARG]... Pass remaining arguments to the interpreted program";

    "-dcone",
    Arg.Set Pipeline.dump_cone,
    " Dump internal ConE representation";

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
    " Make internal errors more verbose (for debugging only)";

    "-L",
    Arg.String (fun p -> cli_lib_search_dirs := p :: !cli_lib_search_dirs),
    "PATH Add PATH to library search directories";

    "-I",
    Arg.String (fun p -> cli_local_search_dirs := p :: !cli_local_search_dirs),
    "PATH Add PATH to local search directories";

    "-load",
    Arg.String DblConfig.load_file_at_startup,
    "FILE Load definitions from FILE at startup (only in REPL mode)";

    "-no-error-context",
    Arg.Clear DblConfig.display_error_context,
    " Do not print piece of code with error, just filename:line:character";

    "-color",
    Arg.Symbol (
      [ "always"; "never"; "auto"; ],
      (fun s -> DblConfig.print_colors_of_string s)),
    " Use colors when printing Errors.";

    "-test",
    Arg.Set DblConfig.test_tagless,
    " Run tagless tests";

    "-test-tags",
    Arg.String 
      (fun s -> String.split_on_char ',' s 
      |> List.map String.trim 
      |> List.map DblConfig.compile_glob 
      |> fun s -> DblConfig.test_globs := !DblConfig.test_globs @ s),
    "GLOBS Run tagged tests matching GLOBS (comma-separated list of globs)";
  
    "-no-show-printing",
    Arg.Clear DblConfig.repl_show_printing,
    " Disable REPL from using method `show' for pretty-printing.";
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
  include_cli_search_dirs ();
  try
    match !fname with
    | None       -> Pipeline.run_repl ()
    | Some _ when !DblConfig.force_repl ->
      Printf.eprintf
        "A file was provided, but other options force REPL mode.\n\
        See %s -help for details.\n"
        Sys.argv.(0);
      exit 2
    | Some fname -> Pipeline.run_file fname
  with
  | InterpLib.Error.Fatal_error -> exit 1
  | Eval.Runtime_error -> exit 2
