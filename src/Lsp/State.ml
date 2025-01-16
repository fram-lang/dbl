(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Server state *)

module UriMap = Map.Make(Uri)

type document = {
  temp_path: string;
}

type connection = {
  in_channel: in_channel;
  out_channel: out_channel;
}

type t = {
  connection: connection;
  documents: document UriMap.t
}

let create ~in_channel ~out_channel =
  {
    connection = { in_channel; out_channel };
    documents = UriMap.empty;
  }

let out_channel { connection = { out_channel; _ }; _ } = out_channel
let in_channel { connection = { in_channel; _ }; _ } = in_channel

let open_document state uri content =
  let temp_path = Filename.temp_file "" DblConfig.src_extension in
  Out_channel.with_open_text temp_path (fun oc ->
    output_string oc content
  );
  let document = { temp_path } in
  { state with documents = UriMap.add uri document state.documents }

let update_document state uri new_content =
  (* Only open documents can be updated. If uri is not found then we want
    to fail because it's more likely to be an error on our side. *)
  let doc = UriMap.find uri state.documents in
  (* overwrites the file if it exists *)
  Out_channel.with_open_text doc.temp_path (fun oc ->
    output_string oc new_content
  ); state

let close_document state uri =
  (* Same as above. *)
  let doc = UriMap.find uri state.documents in
  Sys.remove doc.temp_path;
  { state with documents = UriMap.remove uri state.documents }

let close_all_documents state =
  UriMap.iter (fun _ doc -> Sys.remove doc.temp_path) state.documents;
  { state with documents = UriMap.empty }

let get_document_path state uri =
  (* The client might send requests involving closed files, so we don't fail.
    See State.mli for details. *)
  match UriMap.find_opt uri state.documents with
  | None -> Uri.path uri
  | Some doc -> doc.temp_path

