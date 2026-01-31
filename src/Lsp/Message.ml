(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** JSON <--> OCaml translation of messages *)

(** For each type LspType defined in the LSP specification this file contains:
     - a corresponding OCaml type lsp_type
     - a subset of the following:
        * a function converting from json to lsp_type,
          named to_lsp_type (to match Yojson naming convention).
        * a function converting from lsp_type to json,
          named from_lsp_type (exception: json_of_message and json_of_response,
            which are accessible from outside)
        * a smart constructor named make_lsp_type.
        * additional functions converting from other_type to lsp_type,
          named lsp_type_of_other_type.

  Spec: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
*)

open Yojson.Safe.Util
open Either

let ok = Result.ok

(* ------------------------------------------------------------------------- *)
type json = Yojson.Safe.t

(** Make an Assoc (JSON object), without nulls *)
let make_assoc xs = `Assoc (List.filter (fun (_, v) -> v <> `Null) xs)

let from_option (f : 'a -> json) option =
  match option with
  | None -> `Null
  | Some x -> f x

let from_int (n : int) = `Int n

let from_string (s : string) = `String s

let from_list f xs = `List (List.map f xs)

let from_json (json : json) = json

(* ------------------------------------------------------------------------- *)
type uri = Uri.t

let to_uri json = json |> to_string     |> Uri.of_string

let from_uri uri = uri |> Uri.to_string |> from_string

(* ------------------------------------------------------------------------- *)
type position = {
  line      : int;
  character : int;
}

let to_position json =
  let line      = json |> member "line"      |> to_int in
  let character = json |> member "character" |> to_int in
  { line; character }

let from_position position =
  make_assoc [
    ("line",      from_int position.line);
    ("character", from_int position.character);
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

let from_range range =
  make_assoc [
    ("start", from_position range.start);
    ("end",   from_position range.end_);
  ]

let range_of_position (pos : Position.t) =
  match pos.pos_fname with
  | "<nowhere>" ->
    { start =
      { line = 0;
        character = 0;
      };
      end_ =
      { line = 0;
        character = 0;
      }
    }
  | _ ->
    { start =
      { line      = pos.pos_start_line - 1;
        character = Position.start_column pos - 1
      };
      end_ =
      { line      = pos.pos_end_line - 1;
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
  let range = json |> member "range" |> to_option to_range in
  let text  = json |> member "text"  |> to_string          in
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

let from_location loc =
  make_assoc [
    ("uri",   from_uri loc.uri);
    ("range", from_range loc.range);
  ]

(* ------------------------------------------------------------------------- *)
type diagnostic_severity =
  | Error       (* 1 *)
  | Warning     (* 2 *)
  | Information (* 3 *)
  | Hint        (* 4 *)

let from_diagnostic_severity severity =
  match severity with
  | Error       -> from_int 1
  | Warning     -> from_int 2
  | Information -> from_int 3
  | Hint        -> from_int 4

(** Translate our internal error_class type to diagnostic_severity *)
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

let from_diagnostic_tag tag =
  match tag with
  | Unnecessary -> from_int 1
  | Deprecated  -> from_int 2

(* ------------------------------------------------------------------------- *)
type diagnostic_related_information = {
  location : location;
  message  : string;
}

let from_diagnostic_related_information ri =
  make_assoc [
    ("location", from_location ri.location);
    ("message",  from_string ri.message);
  ]

(* ------------------------------------------------------------------------- *)
type code_description = {
  href : string;
}

let from_code_description cd =
  make_assoc [
    ("href", from_string cd.href);
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
  data                : json option;
}

let make_diagnostic
  ?severity ?code ?code_description ?source ?tags ?related_information ?data
  ~range ~message () =
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

let from_diagnostic d =
  let from_code code =
    match code with
    | Left n  -> from_int n
    | Right s -> from_string s
  in
  make_assoc [
    ("range", from_range d.range);
    ("severity", from_option from_diagnostic_severity d.severity);
    ("code", from_option from_code d.code);
    ("codeDescription", from_option from_code_description d.code_description);
    ("source", from_option from_string d.source);
    ("message", from_string d.message);
    ("tags", from_option from_diagnostic_tag d.tags);
    ("relatedInformation",
      from_list from_diagnostic_related_information d.related_information);
    ("data", from_option from_json d.data);
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

let from_publish_diagnostics_params params =
  make_assoc [
    ("uri",         from_uri params.uri);
    ("version",     from_option from_int params.version);
    ("diagnostics", from_list from_diagnostic params.diagnostics);
  ]

(* ------------------------------------------------------------------------- *)
type markup_kind =
  | Plaintext (* "plaintext" *)
  | Markdown  (* "markdown" *)

let from_markup_kind kind =
  match kind with
  | Plaintext -> from_string "plaintext"
  | Markdown  -> from_string "markdown"

(* ------------------------------------------------------------------------- *)
type markup_content = {
  kind  : markup_kind;
  value : string;
}

let from_markup_content content =
  make_assoc [
    ("kind",  from_markup_kind content.kind);
    ("value", from_string content.value);
  ]

(* ------------------------------------------------------------------------- *)
type hover_result = {
  contents : markup_content;
  range    : range option;
}

let from_hover_result result =
  make_assoc [
    ("contents", from_markup_content result.contents);
    ("range",    from_option from_range result.range);
  ]

(* ------------------------------------------------------------------------- *)
type error_code =
  | ParseError           (* -32700 *)
  | InvalidRequest       (* -32600 *)
  | MethodNotFound       (* -32601 *)
  | InvalidParams        (* -32602 *)
  | InternalError        (* -32603 *)
  | ServerNotInitialized (* -32002 *)

let from_error_code code =
  match code with
  | ParseError           -> from_int (-32700)
  | InvalidRequest       -> from_int (-32600)
  | MethodNotFound       -> from_int (-32601)
  | InvalidParams        -> from_int (-32602)
  | InternalError        -> from_int (-32603)
  | ServerNotInitialized -> from_int (-32002)

(* ------------------------------------------------------------------------- *)
type initialize_result = json

(* ------------------------------------------------------------------------- *)
type server_result =
  | Initialize of initialize_result
  | Shutdown
  (* | Hover of hover_result option *)

let from_server_result result =
  match result with
  | Initialize result -> result
  | Shutdown          -> `Null
  (* | Hover result      -> from_option from_hover_result result *)

(* ------------------------------------------------------------------------- *)
type id = (int, string) Either.t

let from_id id =
  match id with
  | Left id  -> from_int id
  | Right id -> from_string id

let to_id json =
  match json with
  | `Null      -> None
  | `Int id    -> Some (Left id)
  | `String id -> Some (Right id)
  | _ -> raise (Type_error ("invalid id type", json))

(* ------------------------------------------------------------------------- *)
(* notification or request *)
type message = {
  jsonrpc     : string;
  id          : id option;
  method_name : string (* json key: method *);
  params      : json;
}

let make_message ?id ~method_name ~params () =
  { jsonrpc = "2.0"; id; method_name; params }

let message_of_string string =
  try
    let json        = Yojson.Safe.from_string string        in
    let jsonrpc     = json |> member "jsonrpc" |> to_string in
    let id          = json |> member "id"      |> to_id     in
    let method_name = json |> member "method"  |> to_string in
    let params      = json |> member "params" in
    Ok { jsonrpc; id; method_name; params }
  with
  | Yojson.Json_error err -> Error (ParseError, err)
  | Type_error (err, _)   -> Error (InvalidRequest, err)

let json_of_message message =
  make_assoc [
    ("jsonrpc", from_string message.jsonrpc);
    ("id",      from_option from_id message.id);
    ("method",  from_string message.method_name);
    ("params",  message.params)
  ]

(* ------------------------------------------------------------------------- *)
(** Most notifications have an assigned direction:
     from client to server (client_notification)
  or from server to client (server_notification).
  However, some notifications can be sent both ways. In such case just make
  the same constructor in both types. *)

type client_notification =
  | Initialized
  | Exit
  | DidOpen of did_open_params
  | DidChange of did_change_params
  | DidClose of did_close_params

let notification_of_message { method_name; params; _ } =
  try
    match method_name with
    | "initialized"            -> Initialized                             |> ok
    | "exit"                   -> Exit                                    |> ok
    | "textDocument/didOpen"   -> DidOpen (to_did_open_params params)     |> ok
    | "textDocument/didChange" -> DidChange (to_did_change_params params) |> ok
    | "textDocument/didClose"  -> DidClose (to_did_close_params params)   |> ok
    | _ -> Error (MethodNotFound, method_name)
  with
  | Type_error (err, _) -> Error (InvalidParams, err)

(* ------------------------------------------------------------------------- *)
type server_notification =
  | PublishDiagnostics of publish_diagnostics_params

let method_name_of_notification notification =
  match notification with
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"

let json_of_notification notification =
  match notification with
  | PublishDiagnostics params -> from_publish_diagnostics_params params

let message_of_notification notification =
  let method_name = method_name_of_notification notification in
  let params      = json_of_notification notification        in
  make_message ~method_name ~params ()

(* ------------------------------------------------------------------------- *)
type request =
  | Initialize (* of initialize_params *)
  | Shutdown
  (* | Hover of hover_params *)

let request_of_message { method_name; params; _ } =
  try
    match method_name with
    | "initialize"         -> Initialize |> ok
    | "shutdown"           -> Shutdown   |> ok
    (* | "textDocument/hover" -> Hover (to_hover_params params) |> ok *)
    | _ -> Error (MethodNotFound, "Method not found: " ^ method_name)
  with
  | Type_error (err, _) -> Error (InvalidParams, err)

(* ------------------------------------------------------------------------- *)
type response_error = {
  code    : error_code;
  message : string;
  data    : json option;
}

let make_response_error ?data ~code ~message () =
  { code; message; data }

let from_response_error error =
  make_assoc [
    ("code",    from_error_code error.code);
    ("message", from_string error.message);
    ("data",    from_option from_json error.data);
  ]

(* ------------------------------------------------------------------------- *)
type response = {
  jsonrpc         : string;
  id              : (int, string) Either.t option;
  result_or_error : (server_result, response_error) result;
}

let make_response ?id ~result_or_error () =
  { jsonrpc = "2.0"; id; result_or_error }

let json_of_response response =
  match response.result_or_error with
  (* Do not use make_assoc.
     id and either result or error fields are required even if null *)
  | Ok result ->
    `Assoc [
      ("jsonrpc", from_string response.jsonrpc);
      ("id",     from_option from_id response.id);
      ("result", from_server_result result);
    ]
  | Error error ->
    `Assoc [
      ("jsonrpc", from_string response.jsonrpc);
      ("id",    from_option from_id response.id);
      ("error", from_response_error error);
    ]
