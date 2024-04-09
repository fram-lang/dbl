(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

open Common

module StrMap = Map.Make(String)

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

let check_type_inst_uniqueness tinsts =
  let name_of (inst : S.type_inst) =
    match fst inst.data with
    | TNAnon   -> assert false
    | TNEffect -> "effect"
    | TNVar x  -> x
  in
  let pos_of (inst : S.type_inst) = inst.pos in
  let on_error ~pos ~ppos (inst : S.type_inst) =
    Error.report (Error.type_inst_redefinition ~pos ~ppos (fst inst.data)) in
  check_uniqueness ~on_error ~name_of ~pos_of tinsts

let check_inst_uniqueness insts =
  let name_of (inst : S.inst) =
    match inst.data with
    | (NLabel, _) -> "label"
    | ((NVar n | NImplicit n), _) -> n
  in
  let pos_of (inst : S.inst) = inst.pos in
  let on_error ~pos ~ppos (inst : S.inst) =
    Error.report (Error.inst_redefinition ~pos ~ppos (fst inst.data)) in
  check_uniqueness ~on_error ~name_of ~pos_of insts

let check_named_type_arg_uniqueness args =
  let name_of (arg : S.named_type_arg) =
    match fst arg.data with
    | TNAnon   -> assert false
    | TNEffect -> "effect"
    | TNVar x  -> x
  in
  let pos_of (arg : S.named_type_arg) = arg.pos in
  let on_error ~pos ~ppos (arg : S.named_type_arg) =
    Error.report (Error.multiple_named_type_args ~pos ~ppos (fst arg.data)) in
  let args =
    List.filter (fun { S.data = (n, _); _ } -> n <> S.TNAnon) args in
  check_uniqueness ~on_error ~name_of ~pos_of args

let check_named_pattern_uniqueness nps =
  let name_of (np : S.named_pattern) =
    match np.data with
    | (NLabel, _) -> "label"
    | ((NVar n | NImplicit n), _) -> n
  in
  let pos_of (np : S.named_pattern) = np.pos in
  let on_error ~pos ~ppos (np : S.named_pattern) =
    Error.report (Error.multiple_inst_patterns ~pos ~ppos (fst np.data)) in
  check_uniqueness ~on_error ~name_of ~pos_of nps

let check_ctor_named_types data_args ctor_args =
  let check_ctor_arg (arg : S.named_type_arg) =
    match fst arg.data with
    | TNAnon  -> ()
    | (TNEffect | TNVar _) as name ->
      let name' = Name.tr_tname name in
      if List.exists (fun (n, _) -> n = name') data_args then
        Error.report (Error.ctor_type_arg_same_as_data_arg ~pos:arg.pos name)
  in
  List.iter check_ctor_arg ctor_args

let check_generalized_named_types ~pos tvars =
  let names =
    List.filter_map
      (fun (name, _) ->
        match name with
        | T.TNAnon   -> None
        | T.TNEffect -> Some (name, "effect")
        | T.TNVar x  -> Some (name, x))
      tvars
  in
  let name_of   = snd in
  let pos_of _  = pos in
  let on_error ~pos ~ppos (name, _) =
    Error.fatal (Error.type_generalized_twice ~pos name) in
  check_uniqueness ~on_error ~name_of ~pos_of names
