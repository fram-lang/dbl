(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

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
  let name_of { S.data = S.CtorDecl(name, _, _, _); _ } = name in
  let pos_of (ctor : S.ctor_decl) = ctor.pos in
  let on_error ~pos ~ppos ctor =
    Error.report (Error.ctor_redefinition ~pos ~ppos (name_of ctor)) in
  check_uniqueness ~on_error ~name_of ~pos_of ctors

let check_inst_uniqueness insts =
  let name_of (inst : S.inst) =
    match inst.data with
    | ((NVar n | NImplicit n), _) -> n
  in
  let pos_of (inst : S.inst) = inst.pos in
  let on_error ~pos ~ppos (inst : S.inst) =
    Error.report (Error.inst_redefinition ~pos ~ppos (fst inst.data)) in
  check_uniqueness ~on_error ~name_of ~pos_of insts

let check_named_pattern_uniqueness nps =
  let name_of (np : S.named_pattern) =
    match np.data with
    | ((NVar n | NImplicit n), _) -> n
  in
  let pos_of (np : S.named_pattern) = np.pos in
  let on_error ~pos ~ppos (np : S.named_pattern) =
    Error.report (Error.multiple_inst_patterns ~pos ~ppos (fst np.data)) in
  check_uniqueness ~on_error ~name_of ~pos_of nps
