(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

open Common

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

let check_uniqueness ~on_error ~name_of ~pos_of xs =
  let rec loop names xs =
    match xs with
    | [] -> ()
    | x :: xs ->
      let name = name_of x in
      begin match StrMap.find_opt name names with
      | None ->
        let names = StrMap.add name (pos_of x) names in
        loop names xs
      | Some ppos ->
        on_error ~pos:(pos_of x) ~ppos x;
        loop names xs
      end
  in
  loop StrMap.empty xs

let check_ctor_uniqueness ctors =
  let name_of (ctor : S.ctor_decl) = ctor.data.cd_name in
  let pos_of  (ctor : S.ctor_decl) = ctor.pos in
  let on_error ~pos ~ppos ctor =
    Error.report (Error.ctor_redefinition ~pos ~ppos (name_of ctor)) in
  check_uniqueness ~on_error ~name_of ~pos_of ctors

let check_unif_named_type_args args =
  let args =
    args |> List.filter_map
      (fun (pos, name, _) ->
        match name with
        | T.TNAnon  -> None
        | T.TNVar x -> Some (pos, x)) in
  let name_of = snd in
  let pos_of  = fst in
  let on_error ~pos ~ppos (_, name) =
    Error.report (Error.multiple_named_type_args ~pos ~ppos name) in
  check_uniqueness ~on_error ~name_of ~pos_of args

let rec check_names ~pp names =
  let rec loop acc names =
    match names with
    | [] -> ()
    | (pos, name) :: names ->
      begin match Name.Map.find_opt name acc with
      | None -> ()
      | Some ppos ->
        Error.report (Error.multiple_named_args ~pos ~ppos ~pp name)
      end;
  in
  loop Name.Map.empty names

let check_generalized_types ~pos gen_tvars tvars =
  let names =
    List.fold_left
      (fun nset (name, _) ->
        match name with
        | T.TNAnon   -> nset
        | T.TNVar x  -> StrSet.add x nset)
      StrSet.empty
      tvars
  in
  let _: StrSet.t =
    List.fold_left
      (fun nset (name, _) ->
        match name with
        | T.TNAnon   -> nset
        | T.TNVar x  ->
          if StrSet.mem x nset then
            Error.report (Error.generalized_type_clash ~pos x);
          StrSet.add x nset)
      names
      gen_tvars
  in
  ()

let check_generalized_names ~pos ~pp gen_named named =
  let check acc (name, _, _) =
    if Name.Set.mem name acc then
      Error.report (Error.generalized_name_clash ~pos ~pp name);
    Name.Set.add name acc
  in
  let _ : Name.Set.t =
    List.fold_left check (named |> List.map fst |> Name.Set.of_list) gen_named
  in
  ()
