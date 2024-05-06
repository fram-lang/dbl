(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the parser *)

type fname = string

let parse_file ?pos ~use_prelude fname =
  let imports, prog = File.parse_defs ?pos fname in
  let make data = { prog with data } in
  let prog = make (Lang.Surface.EDefs(prog.data, make Lang.Surface.EUnit)) in
  Import.prepend_imports ~use_prelude imports prog

let make_nowhere data =
  { Lang.Surface.pos  = Position.nowhere
  ; Lang.Surface.data = data
  }

let rec repl_seq imported () =
  flush stderr;
  Printf.printf "> %!";
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = "<stdin>"
    };
  match YaccParser.repl Lexer.token lexbuf with
  | Raw.REPL_Exit ->
    Printf.printf "\n%!";
    exit 0

  | Raw.REPL_Expr e ->
    let def = make_nowhere (Lang.Surface.DReplExpr(Desugar.tr_expr e)) in
    Seq.Cons([def], repl_seq imported)

  | Raw.REPL_Defs defs ->
      let defs = List.map Desugar.tr_def defs in
      Seq.Cons(defs, repl_seq imported)

  | Raw.REPL_Import import ->
    let imported, defs = Import.import_one imported import in
    Seq.append (List.to_seq (List.map (fun x -> [x]) defs)) (repl_seq imported) ()

  | exception Parsing.Parse_error ->
    Error.fatal (Error.unexpected_token
      (Position.of_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p)
      (Lexing.lexeme lexbuf))

let repl ~use_prelude =
  if use_prelude then
    let imported, prelude_defs = Import.import_prelude () in
    let repl_expr = make_nowhere (Lang.Surface.ERepl (repl_seq imported)) in
    make_nowhere (Lang.Surface.EDefs(prelude_defs, repl_expr))
  else
    make_nowhere (Lang.Surface.ERepl (repl_seq Import.import_set_empty))
