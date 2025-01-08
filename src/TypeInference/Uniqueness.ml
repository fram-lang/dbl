(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

open Common

module StrMap = Map.Make(String)

type unique_name =
  | UNVar         of string
  | UNOptionalVar of string
  | UNImplicit    of string
  | UNMethod      of T.tvar * string

let tr_name name =
  match name with
  | UNVar x         -> T.NVar x
  | UNOptionalVar x -> T.NOptionalVar x
  | UNImplicit x    -> T.NImplicit x
  | UNMethod(_, m)  -> T.NMethod m

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
(*
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
    | ((NVar n | NOptionalVar n | NImplicit n | NMethod n), _) -> n
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
    | ((NVar n | NOptionalVar n | NImplicit n | NMethod n), _) -> n
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
*)

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

let rec check_names ~env names =
  let name_eq name (_, name') =
    match name, name' with
    | (UNVar x | UNOptionalVar x), (UNVar x' | UNOptionalVar x') -> x = x'
    | UNVar _,         _ -> false
    | UNOptionalVar _, _ -> false

    | UNImplicit x, UNImplicit x' -> x = x'
    | UNImplicit _, _ -> false

    | UNMethod(x, m), UNMethod(y, m') ->
      m = m' && T.TVar.equal x y
    | UNMethod _, _ -> false
  in
  let rec loop acc names =
    match names with
    | [] -> ()
    | (pos, name) :: names ->
      begin match List.find_opt (name_eq name) acc with
      | None -> ()
      | Some(ppos, _) ->
        begin match name with
        | UNVar _ | UNOptionalVar _ | UNImplicit _ ->
          Error.report (Error.multiple_named_args ~pos ~ppos (tr_name name))
        | UNMethod(owner, name) ->
          Error.report (Error.multiple_method_args ~env ~pos ~ppos owner name)
        end
      end;
      loop ((pos, name) :: acc) names
  in
  loop [] names
