(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the server. Handle messages from the client. *)

open Message
open JsonRpc
open Result
open Position

(** Response for the Initialize request *)
let initialize_params =
  Yojson.Safe.from_string
    {|
      {
        "capabilities": {
          "textDocumentSync": {
            "openClose": true,
            "change": 1
          }
        },
        "serverInfo": {
          "name": "framls"
        }
      }
    |}

(** Make DBL correctly find modules *)
let set_module_dirs fname =
  DblConfig.lib_search_dirs := [DblConfig.stdlib_path];
  let cur_dir = Filename.dirname fname in
  DblConfig.local_search_dirs := [cur_dir]

(** Custom report function used to gather reports made by the type-checker.
  The report messages sometimes contain a reference to a file
  ("type ... is defined at <path>"), so we make sure to replace
  the temporary path with the real path of the file the user is editing. *)
let report temp_path real_path diags ?pos ~cls message =
  let open InterpLib.Error in
  let temp_path_re = Str.regexp_string temp_path in
  match pos with
  | Some pos when pos.pos_fname = temp_path || pos = nowhere ->
    let severity = diagnostic_severity_of_error_class cls            in
    let range    = range_of_position pos                             in
    let message  = Str.global_replace temp_path_re real_path message in
    let diag     = make_diagnostic ~range ~severity ~message ()      in
    diags := diag :: !diags
  | _ -> ()

(** Type-check a file. It will report all errors to our report function,
  so we don't need the result. *)
let type_check path =
  try
    DblParser.Main.parse_file ~use_prelude:true path
    |> TypeInference.Main.tr_program
    |> ToCore.Main.tr_program ~repl_mode:false
    |> ignore;
  with
    InterpLib.Error.Fatal_error -> ()

(** Type-check given file and send the diagnostics to the client *)
let process_program state uri =
  let real_path   = Uri.path uri                      in
  let temp_path   = State.get_document_path uri state in
  let diagnostics = ref []                            in

  InterpLib.Error.set_report_function (report temp_path real_path diagnostics);
  set_module_dirs real_path;
  type_check temp_path;

  let params =
    make_publish_diagnostics_params
      ~uri ~diagnostics:(List.rev !diagnostics) () in
  JsonRpc.send_notification state (PublishDiagnostics params)

let handle_notification state notification =
  match notification with
  | Initialized ->
    state
  | Exit ->
    exit 0
  | DidOpen { text_document = { uri; text; _ } } ->
    let state = State.open_document uri text state in
    process_program state uri;
    state
  | DidChange { text_document = { uri; _ }; content_changes } ->
    let apply_content_change state change =
      State.update_document uri change.text state in
    let state =
      List.fold_left apply_content_change state content_changes in
    process_program state uri;
    state
  | DidClose { text_document = { uri } } ->
    let state = State.close_document uri state in
    state

let handle_request state (request : request) =
  match request with
  | Initialize ->
    let result : server_result = Initialize initialize_params in
    state, result |> ok
  | Shutdown ->
    let state  = State.close_all_documents state in
    let result : server_result = Shutdown in
    state, result |> ok

let _ =
  let state = State.create ~in_channel:stdin ~out_channel:stdout in
  JsonRpc.run state handle_request handle_notification
