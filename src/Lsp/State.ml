(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Server state *)

module UriMap = Map.Make(String)

type uri = string

(* We keep the latest version of each document in a temp file *)
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
let in_channel { connection = { in_channel; _}; _ } = in_channel

let open_document state uri =
  let real_path = Uri.to_path uri in
  let content =
    if Sys.file_exists real_path
    then
      let real_file = In_channel.open_text real_path in
      In_channel.input_all real_file
    else "" in
  let temp_path, oc = Filename.open_temp_file "" DblConfig.src_extension in
  output_string oc content;
  close_out oc;
  let document = { temp_path } in
  { state with documents = UriMap.add uri document state.documents }

let update_document state uri new_content =
  match UriMap.find_opt uri state.documents with
  | None -> state
  | Some doc ->
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
  | None -> Uri.to_path uri
  | Some doc -> doc.temp_path

