(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main loop. Get a message, handle it, send response. *)

open Message

(** Send a notification to the client *)
val send_notification : State.t -> server_notification -> unit

(** Main loop of the server. The fuction expects:
     - the initial state
     - a handler for requests
     - a handler for notifications *)
val run :
  State.t ->
  (State.t -> request -> State.t * (server_result, response_error) result) ->
  (State.t -> client_notification -> State.t) ->
  'bottom

