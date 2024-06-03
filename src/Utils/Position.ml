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

(** Join to position into single one.
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

module PrettyPrinting =
struct
  type underline_options =
    | NoUnderline
    | UnderlineBegining
    | UnderlineIfOneLine
    | UnderlineAlways

  type options = {
    context : int;
    underline : t -> underline_options;
    add_line_numbers : bool;
  }

  let default_options = {
    context = 1;
    underline = (fun _ -> UnderlineIfOneLine);
    add_line_numbers = true;
  }

end

(** Get text from file from given position *)
let get_text_range ?(options = PrettyPrinting.default_options) ?channel (pos : t)=
  let generate_underline start_cnum len =
    if len <= 0 then "" else
    let underline = String.make len '^' in
    let padding = String.make (start_cnum - 1) ' ' in
    padding ^ underline
  in
  let add_underline (i, line) : (int option * string) Seq.t =
    match options.underline pos, i with
    | NoUnderline, _ -> Seq.return (i, line)
    | (UnderlineIfOneLine | UnderlineAlways), Some j
        when pos.pos_start_line = pos.pos_end_line
        && pos.pos_start_line = j ->
      let underline =
        generate_underline (start_column pos) pos.pos_length in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineIfOneLine, _ -> Seq.return (i, line)
    | UnderlineBegining, Some j
        when pos.pos_start_line = j ->
      let underline =
        generate_underline (start_column pos) 1 in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineBegining, _ -> Seq.return (i, line)
    | UnderlineAlways, Some j
        when j = pos.pos_start_line ->
      let underline =
        generate_underline (start_column pos) (String.length line - start_column pos) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineAlways, Some j
        when j = pos.pos_end_line ->
      let underline =
        generate_underline 0 (end_column pos) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineAlways, Some j
        when j > pos.pos_start_line && j < pos.pos_end_line ->
      let underline =
        generate_underline 0 (String.length line) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineAlways, _ -> Seq.return (i, line)
  in
  let add_line_number end_line =
    let align_to = Float.of_int end_line
      |> Float.log10
      |> Float.to_int
    in
    fun (i, line) ->
      if not options.add_line_numbers then line else
      match i with
      | None -> String.make (align_to + 3) ' ' ^ "| " ^ line
      | Some i ->
       Printf.sprintf " %*d | %s" (align_to + 1) i line
  in
  let ch = match channel with
    | None -> open_in pos.pos_fname
    | Some ch -> ch
  in
  let lines = In_channel.input_lines ch
    |> List.to_seq
    |> Seq.zip (Seq.ints 1 |> Seq.map Option.some)
    |> Seq.drop (pos.pos_start_line - 1 - options.context |> Int.max 0)
    |> Seq.take (pos.pos_end_line - pos.pos_start_line + 1 + 2*options.context)
    |> Seq.flat_map add_underline
    |> Seq.map (add_line_number pos.pos_end_line)
  in
  if Option.is_none channel then close_in ch;
  let pp_file_name = "-> " ^ pos.pos_fname ^ "\n" in
  pp_file_name ^ String.concat "\n" @@ List.of_seq lines



