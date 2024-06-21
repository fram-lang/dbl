(** Pretty printing of text range from Position.t *)
open Position

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

let get_text_range ?(options=default_options) ~repl_input ~color_printer (pos : t) =
  if pos.pos_fname = "<stdin>" && repl_input <> "" then
    get_text_from_repl ~options ~repl_input ~color_printer pos
  else
    get_text_from_file ~options ~color_printer pos


