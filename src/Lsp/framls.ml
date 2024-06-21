(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main module of the server. Handle messages from the client. *)

open Message
open JsonRpc

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

let set_module_dirs ~fname () =
  DblConfig.lib_search_dirs := [DblConfig.stdlib_path];
  let cur_dir = Filename.dirname fname in
  DblConfig.local_search_dirs := [cur_dir]

let report file_path diags ?(pos : Position.t option) ~cls msg =
  let open InterpLib.Error in
  match pos with
  | None -> ()
  | Some pos when pos.pos_fname = file_path ->
    let severity = diagnostic_severity_of_error_class cls in
    let range = range_of_pos pos in
    let diag = make_diagnostic ~range ~severity ~message:msg () in
    diags := diag :: !diags
  | Some _ -> ()

let process_program state uri =
  let path = State.get_document_path state uri in
  let diagnostics = ref [] in
  InterpLib.Error.set_report_function (report path diagnostics);
  set_module_dirs ~fname:(Uri.to_path uri) ();

  begin try
    DblParser.Main.parse_file ~use_prelude:true path
    |> TypeInference.Main.tr_program
    |> ignore;
  with InterpLib.Error.Fatal_error -> () end;
  let params =
    make_publish_diagnostics_params
      ~uri ~diagnostics:(List.rev !diagnostics) () in
  JsonRpc.send_notification state (PublishDiagnostics params)

let handle_notification state notification =
  match notification with
  | Initialized -> state
  | Exit -> exit 0
  | DidOpen params ->
    let new_state = State.open_document state params.text_document.uri in
    process_program new_state params.text_document.uri;
    new_state
  | DidChange params ->
    let apply_content_change state change =
      State.update_document state params.text_document.uri change.text in
    let new_state =
      List.fold_left apply_content_change state params.content_changes in
    process_program new_state params.text_document.uri;
    new_state
  | DidClose params -> State.close_document state params.text_document.uri
  | Unknown _ -> state

let handle_request state (request : client_request)
  : State.t * (server_result, response_error) Either.t =
  match request with
  | Initialize -> state, Left (Initialize initialize_params)
  | Shutdown -> State.close_all_documents state, Left Shutdown
  | Hover params ->
    let error_message = "Method not supported: textDocument/hover" in
    state, make_response_error ~code:MethodNotFound ~message:error_message ()
  | Unknown method_name ->
    let error_message = "Method not supported: " ^ method_name in
    state, make_response_error ~code:MethodNotFound ~message:error_message ()

let _ =
  let state = State.create ~in_channel:stdin ~out_channel:stdout in
  JsonRpc.run state handle_request handle_notification
