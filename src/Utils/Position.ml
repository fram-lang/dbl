(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Location in the source file *)

type t = {
  pos_fname      : string;
    (** File name *)

  pos_start_line : int;
    (** Line, where the position starts (counting from 1) *)

  pos_start_cnum : int;
    (** Number of the first character (counting from 0) *)

  pos_start_bol  : int;
    (** Number of the first character of the first line (counting from 0)

     [pos_start_cnum - pos_start_bol + 1] is a number of the first column *)

  pos_length     : int;
    (** Length of the position *)

  pos_end_line   : int;
    (** The last line of the position *)

  pos_end_bol    : int;
    (** Number of the first character of the last line (counting from 0) *)
}

(** Dummy position *)
let nowhere =
  { pos_fname      = "<nowhere>"
  ; pos_start_line = 1
  ; pos_start_cnum = 0
  ; pos_start_bol  = 0
  ; pos_length     = 0
  ; pos_end_line   = 1
  ; pos_end_bol    = 1
  }

(** Get position given by start end end positions from lexer *)
let of_pp (p1 : Lexing.position) (p2 : Lexing.position) =
  { pos_fname      = p1.pos_fname
  ; pos_start_line = p1.pos_lnum
  ; pos_start_cnum = p1.pos_cnum
  ; pos_start_bol  = p1.pos_bol
  ; pos_length     = p2.pos_cnum - p1.pos_cnum
  ; pos_end_line   = p2.pos_lnum
  ; pos_end_bol    = p2.pos_bol
  }

(** Get position given by position from lexer and its length *)
let of_lexing len (p : Lexing.position) =
  { pos_fname      = p.pos_fname
  ; pos_start_line = p.pos_lnum
  ; pos_start_cnum = p.pos_cnum
  ; pos_start_bol  = p.pos_bol
  ; pos_length     = len
  ; pos_end_line   = p.pos_lnum
  ; pos_end_bol    = p.pos_bol
  }

(** Join two positions into single one.
  It take file name and start from the first parameter and end from the second
  parameter *)
let join p1 p2 =
  { pos_fname      = p1.pos_fname
  ; pos_start_line = p1.pos_start_line
  ; pos_start_cnum = p1.pos_start_cnum
  ; pos_start_bol  = p1.pos_start_bol
  ; pos_length     = p2.pos_start_cnum + p2.pos_length - p1.pos_start_cnum
  ; pos_end_line   = p2.pos_end_line
  ; pos_end_bol    = p2.pos_end_bol
  }

let join_sorted p1 p2 =
  let pos_start_line, pos_start_cnum, pos_start_bol =
    if p1.pos_start_line < p2.pos_start_line then
      p1.pos_start_line, p1.pos_start_cnum, p1.pos_start_bol
    else if p1.pos_start_line > p2.pos_start_line then
      p2.pos_start_line, p2.pos_start_cnum, p2.pos_start_bol
    else if p1.pos_start_line < p2.pos_start_line then
      p1.pos_start_line, p1.pos_start_cnum, p1.pos_start_bol
    else p2.pos_start_line, p2.pos_start_cnum, p2.pos_start_bol
  and pos_end_line, pos_end_bol =
    if p1.pos_end_line > p2.pos_end_line then
      p1.pos_end_line, p1.pos_end_bol
    else if p1.pos_end_line < p2.pos_end_line then
      p2.pos_end_line, p2.pos_end_bol
    else if p1.pos_end_line > p2.pos_end_line then
      p1.pos_end_line, p1.pos_end_bol
    else p2.pos_end_line, p2.pos_end_bol
  and pos_length = abs (p2.pos_start_cnum - p1.pos_start_cnum)
    + max p1.pos_length p2.pos_length
  and pos_fname = p1.pos_fname
  in
  { pos_fname; pos_start_line; pos_start_cnum; pos_start_bol
  ; pos_length; pos_end_line; pos_end_bol }

(** Join list of positions into single one.
  It take file name the head of the list and join the rest.
  Returns Position.nowhere for empty list *)
let join_list = function
  | [] -> nowhere
  | p1 :: ps -> List.fold_left join_sorted p1 ps

(** Get the number of the first column of the position (counting from 1) *)
let start_column pos =
  pos.pos_start_cnum - pos.pos_start_bol + 1

(** Get the number of the last column of the position (inclusive) *)
let end_column pos =
  pos.pos_start_cnum + pos.pos_length - pos.pos_end_bol

(** Pretty-print position as a string *)
let to_string pos =
  if pos.pos_length <= 1 then
    Printf.sprintf "%s:%d:%d"
      pos.pos_fname pos.pos_start_line (start_column pos)
  else if pos.pos_start_line = pos.pos_end_line then
    Printf.sprintf "%s:%d:%d-%d"
      pos.pos_fname pos.pos_start_line (start_column pos) (end_column pos)
  else
    Printf.sprintf "%s:(%d:%d)-(%d:%d)"
      pos.pos_fname
      pos.pos_start_line (start_column pos)
      pos.pos_end_line   (end_column pos)
