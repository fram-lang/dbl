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

module PrettyPrinting
: sig
  (** Options for underlining specified region *)
  type underline_options =
    | NoUnderline
    (** Disable underlining *)

    | UnderlineBegining
    (** Point to only beginning of region *)

    | UnderlineIfOneLine
    (** Underline whole region, but only if specifies only one line *)

    | UnderlineAlways
    (** Underline whole region *)

  type options = {
    context : int;
    (** how many lines before and after region is to be printed *)

    underline : t -> underline_options;
    (** function that depending on region will select underlining option *)

    add_line_numbers : bool;
    (** should line numbers be added *)
  }

  val default_options : options 
  val get_text_range : options:options -> repl_input:string
        -> color_printer:(string -> string) -> t -> string option

end
= struct
  (** Options for underlining specified region *)
  type underline_options =
    | NoUnderline
    (** Disable underlining *)

    | UnderlineBegining
    (** Point to only beginning of region *)

    | UnderlineIfOneLine
    (** Underline whole region, but only if specifies only one line *)

    | UnderlineAlways
    (** Underline whole region *)

  type options = {
    context : int;
    (** how many lines before and after region is to be printed *)

    underline : t -> underline_options;
    (** function that depending on region will select underlining option *)

    add_line_numbers : bool;
    (** should line numbers be added *)
  }

  let default_options = {
    context = 2;
    underline = (fun _ -> UnderlineIfOneLine);
    add_line_numbers = true;
  }

  let find_tabs line =
    String.fold_left (fun (i, acc) c ->
        if c = '\t' then (i+1, i::acc) else (i+1, acc)) (0, []) line
    |> snd

  let generate_underline ~color_printer start_cnum len tabs =
    if len <= 0 then "" else
    let underline = String.make len '^' |> color_printer in
    let padding = String.make (start_cnum - 1) ' ' in
    let f i _ =
      if List.mem i tabs
      then '\t'
      else ' '
    in String.mapi f padding ^ underline

  let add_underline ~options ~pos ~color_printer (i, line) =
    match options.underline pos, i with
    | NoUnderline, _ -> Seq.return (i, line)
    | (UnderlineIfOneLine | UnderlineAlways), Some j
        when pos.pos_start_line = pos.pos_end_line
        && pos.pos_start_line = j ->
      let underline = generate_underline ~color_printer
          (start_column pos) pos.pos_length (find_tabs line) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineIfOneLine, _ -> Seq.return (i, line)
    | UnderlineBegining, Some j
        when pos.pos_start_line = j ->
      let underline = generate_underline ~color_printer
          (start_column pos) 1 (find_tabs line) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineBegining, _ -> Seq.return (i, line)
    | UnderlineAlways, Some j
        when j = pos.pos_start_line ->
      let underline = generate_underline ~color_printer
        (start_column pos)
        (String.length line - start_column pos)
        (find_tabs line) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineAlways, Some j
        when j = pos.pos_end_line ->
      let underline = generate_underline ~color_printer
          0 (end_column pos) (find_tabs line) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineAlways, Some j
        when j > pos.pos_start_line && j < pos.pos_end_line ->
      let underline = generate_underline ~color_printer
          0 (String.length line) (find_tabs line) in
      Seq.cons (i, line) (Seq.return (None, underline))
    | UnderlineAlways, _ -> Seq.return (i, line)

  let add_line_number ~options end_line =
    let align_to = Float.of_int end_line
      |> Float.log10
      |> Float.to_int
    in
    fun (i, line) ->
      if not options.add_line_numbers then
        String.make (align_to + 3) ' ' ^ "| " ^ line
      else
      match i with
      | None -> String.make (align_to + 3) ' ' ^ "| " ^ line
      | Some i ->
       Printf.sprintf " %*d | %s" (align_to + 1) i line

  let process ~pos ~options ~color_printer seq =
    let to_drop = pos.pos_start_line - 1 - options.context in
    let to_take =
        pos.pos_end_line - pos.pos_start_line + 1 +
        2*options.context - (Int.min 0 to_drop) in
    let lines = seq
      |> Seq.zip (Seq.ints 1 |> Seq.map Option.some)
      |> Seq.drop (Int.max to_drop 0)
      |> Seq.take to_take
      |> Seq.flat_map (add_underline ~options ~pos ~color_printer)
      |> Seq.map (add_line_number pos.pos_end_line ~options)
    in
    String.concat "\n" @@ List.of_seq lines

  let get_text_from_repl ~options ~repl_input ~color_printer pos =
    let lines = String.split_on_char '\n' repl_input in
    let options = { options with add_line_numbers=false } in
    let file_chunk = process ~pos ~options
        ~color_printer (List.to_seq lines) in
    Some file_chunk

  let get_text_from_file ~options ~color_printer pos =
    if Fun.negate Sys.file_exists pos.pos_fname then None else
    let get_line fd () = In_channel.input_line fd in
    let process_file fd = process ~pos ~options ~color_printer
        (Seq.of_dispenser (get_line fd)) in
    let file_chunk = In_channel.with_open_text pos.pos_fname process_file in
    let pp_file_name = " -> " ^ pos.pos_fname ^ "\n" in
    Some (pp_file_name ^ file_chunk)

  let get_text_range ~options ~repl_input ~color_printer (pos : t) =
    if pos.pos_fname = "<stdin>" && repl_input <> "" then
      get_text_from_repl ~options ~repl_input ~color_printer pos
    else
      get_text_from_file ~options ~color_printer pos

end

(** Get text from file from given position *)
let get_text_range ?(options = PrettyPrinting.default_options)
      ~repl_input ~color_printer (pos : t) =
    PrettyPrinting.get_text_range ~options ~repl_input ~color_printer pos
