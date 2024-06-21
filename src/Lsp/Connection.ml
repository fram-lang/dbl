(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Base HTML-like protocol *)

open In_channel
open Out_channel

exception Connection_error of string

type headers =
  { content_length: int option
  ; content_type: string option
  }

let parse_header headers line =
  match String.split_on_char ':' line |> List.map String.trim with
  | ["Content-Length"; value] -> begin
    match int_of_string_opt value with
    | None -> raise (Connection_error ("Invalid Content-Length value: " ^ line))
    | Some len -> { headers with content_length = Some len }
    end
  | ["Content-Type"; value] -> { headers with content_type = Some value }
  | _ -> raise (Connection_error ("Invalid header: " ^ line))

let rec collect_headers (ic: in_channel) =
  match input_line ic with
  | None -> raise (Connection_error "Unexpected end of file")
  | Some "\r" -> []
  | Some line -> line :: collect_headers ic

let receive_headers (ic: in_channel) =
  let headers = collect_headers ic in
  List.fold_left parse_header
    { content_length = None; content_type = None }
    headers

let receive_body (ic: in_channel) len =
  match really_input_string ic len with
  | None -> raise (Connection_error "Unexpected end of file")
  | Some body -> body

let receive_string (ic: in_channel) =
  let headers = receive_headers ic in
  match headers.content_length with
  | None -> raise (Connection_error "Missing Content-Length header")
  | Some len -> receive_body ic len

let output_line (oc: out_channel) message =
  let line = message ^ "\r\n" in
  output_string oc line

let send_string (oc: out_channel) message_string =
  let length = String.length message_string in
  output_line oc ("Content-Length: " ^ string_of_int length);
  output_line oc "";
  output_string oc message_string; flush oc

