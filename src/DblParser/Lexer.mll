(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Lexer *)

{
let kw_map =
  let open YaccParser in
  [ "abstr",    KW_ABSTR
  ; "as",       KW_AS
  ; "data",     KW_DATA
  ; "effect",   KW_EFFECT
  ; "effrow",   KW_EFFROW
  ; "else",     KW_ELSE
  ; "end",      KW_END
  ; "extern",   KW_EXTERN
  ; "finally",  KW_FINALLY
  ; "fn",       KW_FN
  ; "handle",   KW_HANDLE
  ; "handler",  KW_HANDLER
  ; "if",       KW_IF
  ; "implicit", KW_IMPLICIT
  ; "import",   KW_IMPORT
  ; "in",       KW_IN
  ; "label",    KW_LABEL
  ; "let",      KW_LET
  ; "match",    KW_MATCH
  ; "method",   KW_METHOD
  ; "module",   KW_MODULE
  ; "of",       KW_OF
  ; "open",     KW_OPEN
  ; "pub",      KW_PUB
  ; "rec",      KW_REC
  ; "return",   KW_RETURN
  ; "then",     KW_THEN
  ; "type",     KW_TYPE
  ; "with",     KW_WITH
  ; "_",        UNDERSCORE
  ] |> List.to_seq |> Hashtbl.of_seq

(* let op_map = Hashtbl.create 32 *)
let op_map =
  let open YaccParser in
  [ "->", ARROW
  ; "|",  BAR
  ; "=>", ARROW2
  ; "|",  BAR
  ; ":",  COLON
  ; ",",  COMMA
  ; ".",  DOT
  ; "=",  EQ
  ; ";;", SEMICOLON2
  ; "/",  SLASH
  ; ">.", GT_DOT
  ] |> List.to_seq |> Hashtbl.of_seq

let tokenize_oper pos str =
  try Hashtbl.find op_map str with
  | Not_found ->
    let long = String.length str >= 2 in
    begin match str.[0] with
    | '?' | '~' when (not long) ->
      Error.fatal (Error.disallowed_op_error
        (Position.of_lexing (String.length str) pos)
        str)
    | ';'                                           -> YaccParser.OP_0 str
    | '<' when (long && str.[1] = '-')              -> YaccParser.OP_20 str
    | ':' when (long && str.[1] = '=')              -> YaccParser.OP_20 str
    | ','                                           -> YaccParser.OP_30 str
    | '|' when (long && str.[1] = '|')              -> YaccParser.OP_40 str
    | '&' when (long && str.[1] = '&')              -> YaccParser.OP_50 str
    | '!' | '=' | '<' | '>' | '|' | '&' | '$' | '?' -> YaccParser.OP_60 str
    | '@' | ':' | '^'                               -> YaccParser.OP_70 str
    | '+' | '-' | '~'                               -> YaccParser.OP_80 str
    | '*' when (long && str.[1] = '*')              -> YaccParser.OP_100 str
    | '*' | '/' | '%' | '.'                         -> YaccParser.OP_90 str
    | _ -> assert false
    end

let tokenize_ident str =
  match Hashtbl.find_opt kw_map str with
  | Some tok -> tok
  | None     -> YaccParser.LID str

let num_regex = Str.regexp
  {|^\(0[bB][01]*\|0[oO][0-7]*\|[0-9]*\|0[xX][0-9a-fA-F]*\)L?$|}

let tokenize_number pos str =
  if not (Str.string_match num_regex str 0) then
    Error.fatal (Error.invalid_number
      (Position.of_lexing (String.length str) pos)
      str)
  else if String.length str > 0 && str.[String.length str - 1] = 'L' then
    let str = String.sub str 0 (String.length str - 1) in
    match Int64.of_string_opt str with
    | Some n -> YaccParser.NUM64 n
    | None   ->
      Error.fatal (Error.number_out_of_bounds
      (Position.of_lexing (String.length str) pos)
      str)
  else
    match int_of_string_opt str with
    | Some n -> YaccParser.NUM n
    | None   ->
      Error.fatal (Error.number_out_of_bounds
      (Position.of_lexing (String.length str) pos)
      str)

let unescape str =
  match str with
  | "\"" | "'" | "\\" -> str.[0]
  | "0" -> '\000'
  | "n" -> '\n'
  | "b" -> '\b'
  | "t" -> '\t'
  | "r" -> '\r'
  | "v" -> '\x0b'
  | "a" -> '\x07'
  | "f" -> '\x0c'
  | _ ->
    assert (str.[0] = 'x' || str.[0] = 'X');
    Char.chr (int_of_string ("0" ^ str))

  let parse_char chr =
    if String.length chr = 1 then
      YaccParser.CHR (chr.[0])
    else
      let escaped = String.sub chr 1 (String.length chr - 1) in
      YaccParser.CHR (unescape escaped)
}

let whitespace = ['\011'-'\r' '\t' ' ']
let digit      = ['0'-'9']
let lid_start  = ['a'-'z' '_']
let uid_start  = ['A'-'Z']
let var_char   = lid_start | uid_start | digit | '\''

let hex_digit = digit | ['a'-'f'] | ['A'-'F']
let escape =
  ['"' ''' '\\' '0' 'n' 'b' 't' 'r' 'v' 'a' 'f']
  | (['x' 'X'] hex_digit hex_digit)

let char =
  (('\\' escape) | _)

let op_char = [
  '<' '>' '&' '$' '?' '!' '@' '^' '+' '-'
  '~' '*' '%' ';' ',' '=' '|' ':' '.' '/'
]

let comment_name = [^'\000'-' ' '\x7f' '{' '}']*

rule token = parse
    whitespace+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "{#" (comment_name as name) { block_comment name lexbuf }
  | "#"  { line_comment lexbuf.Lexing.lex_start_p lexbuf; token lexbuf }
  | '('  { YaccParser.BR_OPN     }
  | ')'  { YaccParser.BR_CLS     }
  | '['  { YaccParser.SBR_OPN    }
  | ']'  { YaccParser.SBR_CLS    }
  | '{'  { YaccParser.CBR_OPN    }
  | '}'  { YaccParser.CBR_CLS    }
  | op_char+ as x { tokenize_oper lexbuf.Lexing.lex_start_p x }
  | lid_start var_char* as x { tokenize_ident x }
  | uid_start var_char* as x { YaccParser.UID x }
  | '~' lid_start var_char* as x { YaccParser.TLID x }
  | '?' (lid_start var_char* as x) { YaccParser.QLID x }
  | '\'' (char as ch) '\'' { parse_char ch }
  | digit var_char* as x { tokenize_number lexbuf.Lexing.lex_start_p x }
  | '"' {
      let buf = Buffer.create 32 in
      string_token lexbuf.Lexing.lex_start_p buf lexbuf
    }
  | eof    { YaccParser.EOF }
  | _ as x {
      Error.fatal (Error.invalid_character
        (Position.of_lexing 1 lexbuf.Lexing.lex_start_p)
        x)
    }

and string_token pos buf = parse
    "\n" {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      string_token pos buf lexbuf
    }
  | '"' {
      lexbuf.Lexing.lex_start_p <- pos;
      YaccParser.STR (Buffer.contents buf)
    }
  | [^'"' '\\']+ as str {
      Buffer.add_string buf str;
      string_token pos buf lexbuf
    }
  | "\\" (escape as esc) {
      Buffer.add_char buf (unescape esc);
      string_token pos buf lexbuf
    }
  | "\\" {
      Error.fatal (Error.invalid_escape_code
        (Position.of_lexing 1 lexbuf.Lexing.lex_start_p))
    }
  | eof {
      Error.fatal (Error.eof_in_string
        (Position.of_lexing 0 lexbuf.Lexing.lex_start_p))
    }

and block_comment name = parse
    '\n' { Lexing.new_line lexbuf; block_comment name lexbuf }
  | (comment_name as name') "#}" {
      if String.ends_with ~suffix:name name' then token lexbuf
      else block_comment name lexbuf
    }
  | comment_name { block_comment name lexbuf }
  | eof {
      Error.fatal (Error.eof_in_comment
        (Position.of_lexing 0 lexbuf.Lexing.lex_start_p)
        name)
    }
  | _ { block_comment name lexbuf }

and line_comment start_p = parse
    "@ " (digit+ as lnum) " " ([^'\n']+ as fname) "\n" {
      Lexing.new_line lexbuf;
      match int_of_string_opt lnum with
      | Some lnum ->
        lexbuf.Lexing.lex_curr_p <-
          { lexbuf.Lexing.lex_curr_p with
            pos_fname = fname;
            pos_lnum  = lnum
          }
      | None ->
        let pos = Position.of_pp start_p lexbuf.Lexing.lex_curr_p in
        Error.warn (
          Error.invalid_lexer_directive
            ~msg:"line number out of range"
            pos)
    }
  | "@" [^'\n']* ("\n"? as nl) {
      let pos = Position.of_pp start_p lexbuf.Lexing.lex_curr_p in
      Error.warn (Error.invalid_lexer_directive pos);
      if nl <> "" then Lexing.new_line lexbuf
    }
  | [^'\n']* ("\n"? as nl) {
      if nl <> "" then Lexing.new_line lexbuf
    }
