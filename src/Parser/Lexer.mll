(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Lexer *)

(* Author: Piotr Polesiuk, 2023,2024 *)

{
let kw_map =
  let open YaccParser in
  [ "data",     KW_DATA
  ; "effect",   KW_EFFECT
  ; "end",      KW_END
  ; "fn",       KW_FN
  ; "handle",   KW_HANDLE
  ; "implicit", KW_IMPLICIT
  ; "in",       KW_IN
  ; "let",      KW_LET
  ; "match",    KW_MATCH
  ; "of",       KW_OF
  ; "with",     KW_WITH
  ; "_",        UNDERSCORE
  ] |> List.to_seq |> Hashtbl.of_seq

let tokenize_ident str =
  match Hashtbl.find_opt kw_map str with
  | Some tok -> tok
  | None     -> YaccParser.LID str
}

let whitespace = ['\011'-'\r' '\t' ' ']
let digit      = ['0'-'9']
let lid_start  = ['a'-'z' '_']
let uid_start  = ['A'-'Z']
let var_char   = lid_start | uid_start | digit | '\''

rule token = parse
    whitespace+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { block_comment 1 lexbuf }
  | "//" { skip_line lexbuf; token lexbuf }
  | '('  { YaccParser.BR_OPN     }
  | ')'  { YaccParser.BR_CLS     }
  | '['  { YaccParser.SBR_OPN    }
  | ']'  { YaccParser.SBR_CLS    }
  | '{'  { YaccParser.CBR_OPN    }
  | '}'  { YaccParser.CBR_CLS    }
  | "->" { YaccParser.ARROW      }
  | "=>" { YaccParser.ARROW2     }
  | "|"  { YaccParser.BAR        }
  | ":"  { YaccParser.COLON      }
  | ","  { YaccParser.COMMA      }
  | "="  { YaccParser.EQ         }
  | ";;" { YaccParser.SEMICOLON2 }
  | "/"  { YaccParser.SLASH      }
  | lid_start var_char* as x { tokenize_ident x }
  | uid_start var_char* as x { YaccParser.UID x }
  | '`' lid_start var_char* as x { YaccParser.TLID x }
  | eof    { YaccParser.EOF }
  | _ as x {
      Error.fatal (Error.invalid_character
        (Position.of_lexing 1 lexbuf.Lexing.lex_curr_p)
        x)
    }

and block_comment depth = parse
    '\n' { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | "(*" { block_comment (depth+1) lexbuf }
  | "*)" {
      if depth = 1 then token lexbuf
      else block_comment (depth-1) lexbuf
    }
  | "//" { skip_line lexbuf; block_comment depth lexbuf }
  | eof {
      Error.fatal (Error.eof_in_comment
        (Position.of_lexing 0 lexbuf.Lexing.lex_curr_p))
    }
  | _ { block_comment depth lexbuf }

and skip_line = parse
    '\n' { Lexing.new_line lexbuf }
  | eof  { () }
  | _    { skip_line lexbuf }
