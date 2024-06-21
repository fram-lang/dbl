(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Main loop. Get message, handle it, send response. *)

open Message

val send_response : State.t -> server_response -> unit

val send_message : State.t -> message -> unit

val send_notification : State.t -> server_notification -> unit

val run :
  State.t ->
  (State.t -> client_request -> State.t * (server_result, response_error) Either.t) ->
  (State.t -> client_notification -> State.t) ->
  'bottom

