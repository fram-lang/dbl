(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** JSON <--> OCaml translation of messages *)

(** This module contains OCaml definitions of types defined
  in the LSP specification, together with needed conversion functions
  and smart constructors. *)

type json = Yojson.Safe.t

type uri = Uri.t

type position = {
  line      : int;
  character : int;
}

type range = {
  start : position;
  end_  : position (* json key: end *);
}

val range_of_position : Position.t -> range

type text_document_identifier = {
  uri: uri
}

type progress_token = (int, string) Either.t

type hover_params = {
  text_document   : text_document_identifier;
  position        : position;
  work_done_token : progress_token option;
}

type text_document_item = {
  uri         : uri;
  language_id : string;
  version     : int;
  text        : string
}

type did_open_params = {
  text_document: text_document_item
}

type versioned_text_document_identifier = {
  version : int;
  uri     : uri
}

type text_document_content_change_event = {
  range : range option;
  text  : string
}

type did_change_params = {
  text_document   : versioned_text_document_identifier;
  content_changes : text_document_content_change_event list;
}

type did_close_params = {
  text_document : text_document_identifier
}

type location = {
  uri   : uri;
  range : range;
}

type diagnostic_severity =
  | Error
  | Warning
  | Information
  | Hint

(* Translate our internal error_class type to diagnostic_severity *)
val diagnostic_severity_of_error_class :
  InterpLib.Error.error_class -> diagnostic_severity

type diagnostic_tag =
  | Unnecessary
  | Deprecated

type diagnostic_related_information = {
  location : location;
  message  : string;
}

type code_description = {
  href : string;
}

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

val make_diagnostic :
  ?severity:diagnostic_severity ->
  ?code:(int, string) Either.t ->
  ?code_description:code_description ->
  ?source:string ->
  ?tags:diagnostic_tag ->
  ?related_information:diagnostic_related_information list ->
  ?data:json ->
  range:range ->
  message:string ->
  unit ->
  diagnostic

type publish_diagnostics_params = {
  uri         : uri;
  version     : int option;
  diagnostics : diagnostic list;
}

val make_publish_diagnostics_params :
  ?version:int ->
  uri:uri ->
  diagnostics:diagnostic list ->
  unit ->
  publish_diagnostics_params

type markup_kind =
  | Plaintext
  | Markdown

type markup_content = {
  kind  : markup_kind;
  value : string;
}

type hover_result = {
  contents : markup_content;
  range    : range option;
}

type error_code =
  | ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerNotInitialized

type initialize_result = json

type server_result =
  | Initialize of initialize_result
  | Shutdown

type id = (int, string) Either.t

type message = {
  jsonrpc     : string;
  id          : id option;
  method_name : string (* json key: method *);
  params      : json;
}

val make_message :
  ?id:id ->
  method_name:string ->
  params:json ->
  unit ->
  message

val message_of_string : string -> (message, error_code * string) result
val json_of_message   : message -> json

type client_notification =
  | Initialized
  | Exit
  | DidOpen of did_open_params
  | DidChange of did_change_params
  | DidClose of did_close_params

val notification_of_message :
  message -> (client_notification, error_code * string) result

type server_notification =
  | PublishDiagnostics of publish_diagnostics_params

val message_of_notification : server_notification -> message

type request =
  | Initialize
  | Shutdown

val request_of_message : message -> (request, error_code * string) result

type response_error = {
  code    : error_code;
  message : string;
  data    : json option;
}

val make_response_error :
  ?data:json -> code:error_code -> message:string -> unit -> response_error

type response = {
  jsonrpc         : string;
  id              : (int, string) Either.t option;
  result_or_error : (server_result, response_error) result;
}

val make_response :
  ?id:(int, string) Either.t ->
  result_or_error:(server_result, response_error) result ->
  unit ->
  response

val json_of_response : response -> json
