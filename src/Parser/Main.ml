(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the parser *)

(* 2023: Piotr Polesiuk: initial implementation
   2024: Piotr Polesiuk, Patrycja Balik: minor changes *)

type fname = string

let with_in_channel ?pos fname func =
  match open_in fname with
  | chan ->
    begin match func chan with
    | result -> close_in_noerr chan; result
    | exception Sys_error msg ->
      close_in_noerr chan;
      Error.fatal (Error.cannot_read_file ?pos ~fname msg)
    | exception ex ->
      close_in_noerr chan;
      raise ex
    end
  | exception Sys_error msg ->
    Error.fatal (Error.cannot_open_file ?pos ~fname msg)

let parse_defs ?pos fname =
  with_in_channel ?pos fname (fun chan ->
    let lexbuf = Lexing.from_channel chan in
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with
        Lexing.pos_fname = fname
      };
    try YaccParser.file Lexer.token lexbuf with
    | Parsing.Parse_error ->
      Error.fatal (Error.unexpected_token
        (Position.of_pp
          lexbuf.Lexing.lex_start_p
          lexbuf.Lexing.lex_curr_p)
        (Lexing.lexeme lexbuf)))
  |> Desugar.tr_program

let parse_lib fname (e : Lang.Surface.program) =
  let p = parse_defs fname in
  let make data = { e with data = data } in
  Lang.Surface.(make (EDefs(p.data, e)))

let parse_file ?pos fname =
  let p = parse_defs ?pos fname in
  let make data = { p with data = data } in
  Lang.Surface.(make (EDefs(p.data, make EUnit)))

let make_nowhere data =
  { Lang.Surface.pos  = Position.nowhere
  ; Lang.Surface.data = data
  }

let rec repl_seq () =
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
    Seq.Cons(def, repl_seq)

  | Raw.REPL_Def def ->
    Seq.Cons(Desugar.tr_def def, repl_seq)

  | exception Parsing.Parse_error ->
    Error.fatal (Error.unexpected_token
      (Position.of_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p)
      (Lexing.lexeme lexbuf))

let repl = make_nowhere (Lang.Surface.ERepl repl_seq)
