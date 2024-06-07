(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main loop. Get message, handle it, send response. *)

open Connection
open Message

let send_response state response =
  let response_json = json_of_response response in
  let response_string = Yojson.Safe.to_string response_json in
  send_string (State.out_channel state) response_string

let send_message state message =
  let message_json = json_of_message message in
  let message_string = Yojson.Safe.to_string message_json in
  send_string (State.out_channel state) message_string

let send_notification state notification =
  let message = message_of_notification notification in
  send_message state message

let rec run (state : State.t) handle_request handle_notification =
  let rec loop state =
    let open Yojson.Safe.Util in

    let ic = State.in_channel state in

    let message_raw = receive_string ic in
    match parse_message_option message_raw with
    | None ->
      let response = make_response
        (make_response_error ~code:ParseError ~message:"Parse error" ()) in
      send_response state response;
      loop state
    | Some { id; method_name; params; _ } ->
      try match id with
      | None ->
        let notification = parse_notification method_name params in
        let state = handle_notification state notification in
        loop state
      | Some id ->
        let request = parse_request method_name params in
        let state, result_or_error = handle_request state request in
        let response = make_response ~id result_or_error in
        send_response state response;
        loop state
      with
      (* Only send error response if the message was a Request *)
      | Type_error _ when Option.is_some id ->
        let error = make_response_error
            ~code:InvalidParams
            ~message:"Invalid params" () in
        let response = make_response ?id error in
        send_response state response;
        loop state
      (* The server MUST NOT reply to a Notification even in case of an error *)
      | Type_error _ ->
        loop state
  in loop state

