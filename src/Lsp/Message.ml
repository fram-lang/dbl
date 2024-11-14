(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** JSON <--> OCaml translation of messages *)

open Yojson.Safe.Util
open Either

let make_assoc xs = `Assoc (List.filter (fun (_, v) -> v <> `Null) xs)

let json_of_option f option =
  match option with
  | None -> `Null
  | Some x -> f x
let json_of_int n = `Int n
let json_of_string s = `String s
let json_of_json json = json

(* ------------------------------------------------------------------------- *)
type uri = string
let to_uri json = json |> to_string

(* ------------------------------------------------------------------------- *)
type position = {
  line      : int;
  character : int;
}

let to_position json =
  let line      = json |> member "line"      |> to_int in
  let character = json |> member "character" |> to_int in
  { line; character }

let json_of_position position =
  make_assoc [
    ("line",      `Int position.line);
    ("character", `Int position.character);
  ]

(* ------------------------------------------------------------------------- *)
type range = {
  start : position;
  end_  : position (* json key: end *);

}

let to_range json =
  let start = json |> member "start" |> to_position in
  let end_  = json |> member "end"   |> to_position in
  { start; end_ }

let to_range_option json =
  match json with
  | `Null -> None
  | _ -> Some (to_range json)

let json_of_range range =
  make_assoc [
    ("start", json_of_position range.start);
    ("end",   json_of_position range.end_);
  ]

let range_of_pos (pos : Position.t) =
  {
    start = {
      line      = pos.pos_start_line - 1;
      character = Position.start_column pos - 1
    };
    end_  = {
      line      = pos.pos_end_line - 1;
      character = Position.end_column pos
    };
  }

(* ------------------------------------------------------------------------- *)
type text_document_identifier = {
  uri: uri
}

let to_text_document_identifier json =
  let uri = json |> member "uri" |> to_uri in
  { uri }

(* ------------------------------------------------------------------------- *)
type progress_token = (int, string) Either.t

let to_progress_token_option json =
  match json with
  | `Null         -> None
  | `Int token    -> Some (Left token)
  | `String token -> Some (Right token)
  | _ -> raise (Type_error ("invalid progress token type", json))

(* ------------------------------------------------------------------------- *)
type hover_params = {
  text_document   : text_document_identifier;
  position        : position;
  work_done_token : progress_token option;
}

let to_hover_params json =
  let text_document =
    json |> member "textDocument"  |> to_text_document_identifier in
  let position = 
    json |> member "position"      |> to_position                 in
  let work_done_token =
    json |> member "workDoneToken" |> to_progress_token_option    in
  { text_document; position; work_done_token }

(* ------------------------------------------------------------------------- *)
type text_document_item = {
  uri         : uri;
  language_id : string;
  version     : int;
  text        : string
}

let to_text_document_item json =
  let uri         = json |> member "uri"        |> to_uri    in
  let language_id = json |> member "languageId" |> to_string in
  let version     = json |> member "version"    |> to_int    in
  let text        = json |> member "text"       |> to_string in
  { uri; language_id; version; text }

(* ------------------------------------------------------------------------- *)
type did_open_params = {
  text_document: text_document_item
}

let to_did_open_params json =
  let text_document = json |> member "textDocument" |> to_text_document_item in
  { text_document }

(* ------------------------------------------------------------------------- *)
type versioned_text_document_identifier = {
  version : int;
  uri     : uri
}

let to_versioned_text_document_identifier json =
  let version = json |> member "version" |> to_int in
  let uri     = json |> member "uri"     |> to_uri in
  { version; uri }

(* ------------------------------------------------------------------------- *)
type text_document_content_change_event = {
  range : range option;
  text  : string
}

let to_text_document_content_change_event json =
  let range = json |> member "range" |> to_range_option in
  let text  = json |> member "text"  |> to_string       in
  { range; text }

(* ------------------------------------------------------------------------- *)
type did_change_params = {
  text_document   : versioned_text_document_identifier;
  content_changes : text_document_content_change_event list;
}

let to_did_change_params json =
  let text_document   =
    json |> member "textDocument"   |> to_versioned_text_document_identifier in
  let content_changes =
    json |> member "contentChanges" |> to_list
    |> List.map to_text_document_content_change_event in
  { text_document; content_changes }

(* ------------------------------------------------------------------------- *)
type did_close_params = {
  text_document : text_document_identifier
}

let to_did_close_params json =
  let text_document =
    json |> member "textDocument" |> to_text_document_identifier in
  { text_document }

(* ------------------------------------------------------------------------- *)
type location = {
  uri   : uri;
  range : range;
}

let json_of_location loc =
  make_assoc [
    ("uri",   `String loc.uri);
    ("range", json_of_range loc.range);
  ]

(* ------------------------------------------------------------------------- *)
type diagnostic_severity =
  | Error       (* 1 *)
  | Warning     (* 2 *)
  | Information (* 3 *)
  | Hint        (* 4 *)

let json_of_diagnostic_severity severity =
  match severity with
  | Error       -> `Int 1
  | Warning     -> `Int 2
  | Information -> `Int 3
  | Hint        -> `Int 4

(* Translate our internal error_class type to diagnostic_severity *)
let diagnostic_severity_of_error_class cls : diagnostic_severity =
  let open InterpLib.Error in
  match cls with
  | FatalError -> Error
  | Error      -> Error
  | Warning    -> Warning
  | Note       -> Information

(* ------------------------------------------------------------------------- *)
type diagnostic_tag =
  | Unnecessary (* 1 *)
  | Deprecated  (* 2 *)

let json_of_diagnostic_tag tag =
  match tag with
  | Unnecessary -> `Int 1
  | Deprecated  -> `Int 2

(* ------------------------------------------------------------------------- *)
type diagnostic_related_information = {
  location : location;
  message  : string;
}

let json_of_diagnostic_related_information ri =
  make_assoc [
    ("location", json_of_location ri.location);
    ("message",  `String ri.message);
  ]

(* ------------------------------------------------------------------------- *)
type code_description = {
  href : string;
}

let json_of_code_description cd =
  make_assoc [
    ("href", `String cd.href);
  ]

(* ------------------------------------------------------------------------- *)
type diagnostic = {
  range               : range;
  severity            : diagnostic_severity option;
  code                : (int, string) Either.t option;
  code_description    : code_description option;
  source              : string option;
  message             : string;
  tags                : diagnostic_tag option;
  related_information : diagnostic_related_information list;
  data                : Yojson.Safe.t option;
}

let make_diagnostic
  ?severity ?code ?code_description ?source ?tags ?related_information ?data
  ~message ~range () =
  {
    range;
    severity;
    code;
    code_description;
    source;
    message;
    tags;
    related_information = related_information |> Option.value ~default:[];
    data;
  }

let json_of_diagnostic d =
  let json_of_code code =
    match code with
    | Left n  -> `Int n
    | Right s -> `String s
  in
  make_assoc [
    ("range", json_of_range d.range);
    ("severity", json_of_option json_of_diagnostic_severity d.severity);
    ("code", json_of_option json_of_code d.code);
    ("codeDescription", json_of_option json_of_code_description d.code_description);
    ("source", json_of_option json_of_string d.source);
    ("message", `String d.message);
    ("tags", json_of_option json_of_diagnostic_tag d.tags);
    ("relatedInformation", `List (List.map json_of_diagnostic_related_information d.related_information));
    ("data", json_of_option json_of_json d.data);
  ]

(* ------------------------------------------------------------------------- *)
type publish_diagnostics_params = {
  uri         : uri;
  version     : int option;
  diagnostics : diagnostic list;
}

let make_publish_diagnostics_params ?version ~uri ~diagnostics () =
  {
    uri;
    version;
    diagnostics;
  }

let json_of_publish_diagnostics_params params =
  make_assoc [
    ("uri",         `String params.uri);
    ("version",     json_of_option json_of_int params.version);
    ("diagnostics", `List (List.map json_of_diagnostic params.diagnostics));
  ]

(* ------------------------------------------------------------------------- *)
type client_request =
  | Initialize (* TODO: of initialize_params *)
  | Shutdown
  | Hover of hover_params
  | Unknown of string

let parse_request method_name params =
  match method_name with
  | "initialize"         -> Initialize
  | "shutdown"           -> Shutdown
  | "textDocument/hover" -> Hover (to_hover_params params)
  | _ -> Unknown method_name

(* ------------------------------------------------------------------------- *)
type client_notification =
  | Initialized
  | Exit
  | DidOpen of did_open_params
  | DidChange of did_change_params
  | DidClose of did_close_params
  | Unknown of string

let parse_notification method_name params =
  match method_name with
  | "initialized"            -> Initialized
  | "exit"                   -> Exit
  | "textDocument/didOpen"   -> DidOpen (to_did_open_params params)
  | "textDocument/didChange" -> DidChange (to_did_change_params params)
  | "textDocument/didClose"  -> DidClose (to_did_close_params params)
  | _ -> Unknown method_name

(* ------------------------------------------------------------------------- *)
type server_notification =
  | PublishDiagnostics of publish_diagnostics_params

let method_name_of_notification notification =
  match notification with
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"

let json_of_notification notification =
  match notification with
  | PublishDiagnostics params -> json_of_publish_diagnostics_params params

(* ------------------------------------------------------------------------- *)
type markup_kind =
  | PlainText (* "plaintext" *)
  | Markdown  (* "markdown" *)

let json_of_markup_kind kind =
  match kind with
  | PlainText -> `String "plaintext"
  | Markdown  -> `String "markdown"

(* ------------------------------------------------------------------------- *)
type markup_content = {
  kind  : markup_kind;
  value : string;
}

let json_of_markup_content content =
  make_assoc [
    ("kind",  json_of_markup_kind content.kind);
    ("value", `String content.value);
  ]

(* ------------------------------------------------------------------------- *)
type hover_result = {
  contents : markup_content;
  range    : range option;
}

let json_of_hover_result result =
  make_assoc [
    ("contents", json_of_markup_content result.contents);
    ("range",    json_of_option json_of_range result.range);
  ]

(* ------------------------------------------------------------------------- *)
type initialize_result = Yojson.Safe.t

(* ------------------------------------------------------------------------- *)
type server_result =
  | Initialize of initialize_result
  | Shutdown
  | Hover of hover_result option

let json_of_server_result result =
  match result with
  | Initialize result -> result
  | Shutdown          -> `Null
  | Hover result      -> json_of_option json_of_hover_result result

(* ------------------------------------------------------------------------- *)
type id = (int, string) Either.t

let json_of_id id =
  match id with
  | Left id  -> `Int id
  | Right id -> `String id

(* ------------------------------------------------------------------------- *)
(* notification or request *)
type message = {
  jsonrpc     : string;
  id          : id option;
  method_name : string (* json key: method *);
  params      : Yojson.Safe.t;
}

let make_message ?(jsonrpc="2.0") ?id ~method_name ~params () =
  { jsonrpc; id; method_name; params }

let parse_message_option message =
  try
    let json    = Yojson.Safe.from_string message in
    let jsonrpc = json |> member "jsonrpc" |> to_string in
    let id =
      match json |> member "id" with
      | `Null      -> None
      | `Int id    -> Some (Left id)
      | `String id -> Some (Right id)
      | _ -> raise (Type_error ("invalid id type", json))
    in
    let method_name = json |> member "method" |> to_string in
    let params      = json |> member "params" in
    Some { jsonrpc; id; method_name; params }
  with
    | Type_error _
    | Yojson.Json_error _ -> None

let json_of_message message =
  make_assoc [
    ("jsonrpc", `String message.jsonrpc);
    ("id",      json_of_option json_of_id message.id);
    ("method",  `String message.method_name);
    ("params",  message.params)
  ]

let message_of_notification notification =
  let method_name = method_name_of_notification notification in
  let params      = json_of_notification notification        in
  make_message ~method_name ~params ()

(* ------------------------------------------------------------------------- *)
type error_code =
  | ParseError           (* -32700 *)
  | InvalidRequest       (* -32600 *)
  | MethodNotFound       (* -32601 *)
  | InvalidParams        (* -32602 *)
  | InternalError        (* -32603 *)
  | ServerNotInitialized (* -32002 *)

let json_of_error_code code =
  match code with
  | ParseError           -> `Int (-32700)
  | InvalidRequest       -> `Int (-32600)
  | MethodNotFound       -> `Int (-32601)
  | InvalidParams        -> `Int (-32602)
  | InternalError        -> `Int (-32603)
  | ServerNotInitialized -> `Int (-32002)

(* ------------------------------------------------------------------------- *)
type response_error = {
  code    : error_code;
  message : string;
  data    : Yojson.Safe.t option;
}

let make_response_error ?data ~code ~message () =
  Right { code; message; data }

let json_of_response_error error =
  make_assoc [
    ("code",    json_of_error_code error.code);
    ("message", `String error.message);
    ("data",    json_of_option json_of_json error.data);
  ]

(* ------------------------------------------------------------------------- *)
type server_response = {
  id              : (int, string) Either.t option;
  result_or_error : (server_result, response_error) Either.t;
}

let make_response ?id result_or_error =
  { id; result_or_error }

let json_of_response response =
  match response.result_or_error with
  | Left result ->
    (* Do not use make_assoc. id and result fields are required even if null *)
    `Assoc [
      ("id",     json_of_option json_of_id response.id);
      ("result", json_of_server_result result);
    ]
  | Right error ->
    `Assoc [
      ("id",    json_of_option json_of_id response.id);
      ("error", json_of_response_error error);
    ]
