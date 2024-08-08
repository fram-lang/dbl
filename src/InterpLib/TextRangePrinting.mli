(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* ========================================================================= *)
(** Printing to terminal with colors *)
type printing_color =
  | Red
  | Teal
  | Yellow
  | Black
  | Green
  | Blue
  | Magenta
  | Cyan
  | White
  | Default

val color_string : printing_color -> string -> string
val bolden_string : string -> string
val underline_string : string -> string

(* ========================================================================= *)
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

  underline : Position.t -> underline_options;
  (** function that depending on region will select underlining option *)

  add_line_numbers : bool;
  (** should line numbers be added *)
}

val default_options : options 

val get_text_range : ?options:options -> repl_input:string
      -> color:printing_color -> Position.t -> string option
