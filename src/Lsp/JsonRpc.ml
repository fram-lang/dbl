(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main loop. Get a message, handle it, send response. *)

open Connection
open Message

(* ------------------------------------------------------------------------- *)
(* Auxiliary functions doing type -> json -> string conversion
  and sending it to the output *)

let send_json state json =
  let string = Yojson.Safe.to_string json in
  send_string (State.out_channel state) string

let send_response state response =
  send_json state (json_of_response response)

let send_message state message =
  send_json state (json_of_message message)

let send_notification state notification =
  let message = message_of_notification notification in
  send_message state message

let send_error state ?id code message =
  let error = make_response_error ~code ~message () |> Result.error in
  let response = make_response ?id ~result_or_error:error () in
  send_response state response

(* ------------------------------------------------------------------------- *)
(* Main function *)

let run state handle_request handle_notification =

  (* Handle messages in a loop *)
  let rec loop state =
    let string = receive_string (State.in_channel state) in
    match message_of_string string with
    (* There was an error parsing the message *)
    | Error (code, msg) ->
      send_error state code msg;
      loop state
    (* The message is a notification *)
    | Ok ({ id = None; _ } as message) ->
      begin match notification_of_message message with
      | Ok notification ->
        let state = handle_notification state notification in
        loop state
      (* There was an error interpreting the message as a notification.
        Because notifications don't get a response, we ignore it
        (according to the specification). *)
      | Error _ ->
        loop state
      end
    (* The message is a request *)
    | Ok ({ id = Some id; _ } as message) ->
      begin match request_of_message message with
      | Ok request ->
        let state, result = handle_request state request in
        let response = make_response ~id ~result_or_error:result () in
        send_response state response;
        loop state
      (* There was an error interpreting the message as a request *)
      | Error (code, msg) ->
        send_error state ~id code msg;
        loop state
      end

  in loop state

