(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Server state *)

module UriMap = Map.Make(Uri)

(** We keep the latest version of each document in a temp file *)
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
  match UriMap.find_opt uri state.documents with
  | None -> state
  | Some doc ->
    (* overwrites the file if it exists *)
    Out_channel.with_open_text doc.temp_path (fun oc ->
      output_string oc new_content
    ); state

let close_document state uri =
  match UriMap.find_opt uri state.documents with
  | None -> state
  | Some doc ->
    Sys.remove doc.temp_path;
    { state with documents = UriMap.remove uri state.documents }

let close_all_documents state =
  UriMap.iter (fun _ doc -> Sys.remove doc.temp_path) state.documents;
  { state with documents = UriMap.empty }

let get_document_path state uri =
  match UriMap.find_opt uri state.documents with
  | None -> Uri.path uri
  | Some doc -> doc.temp_path

