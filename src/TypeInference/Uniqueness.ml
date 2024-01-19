(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking uniqueness of various mutual definitions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

module StrMap = Map.Make(String)

type cat =
  | Constructor
  | ExplicitInst
  | InstPattern

let report_error ~cat name pos ppos =
  match cat with
  | Constructor ->
    Error.report (Error.ctor_redefinition ~pos ~ppos name)
  | ExplicitInst ->
    Error.report (Error.inst_redefinition ~pos ~ppos name)
  | InstPattern ->
    Error.report (Error.multiple_inst_patterns ~pos ~ppos name)

let check_uniqueness ~cat ~name_of ~pos_of xs =
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
        report_error ~cat name (pos_of x) ppos;
        loop names xs
      end
  in
  loop StrMap.empty xs

let check_ctor_uniqueness ctors =
  let name_of { S.data = S.CtorDecl(name, _, _, _); _ } = name in
  let pos_of (ctor : S.ctor_decl) = ctor.pos in
  check_uniqueness ~cat:Constructor ~name_of ~pos_of ctors

let check_inst_uniqueness insts =
  let name_of (inst : S.inst) =
    match inst.data with
    | IName(n, _) -> n
  in
  let pos_of (inst : S.inst) = inst.pos in
  check_uniqueness ~cat:ExplicitInst ~name_of ~pos_of insts

let check_inst_pattern_uniqueness ips =
  let name_of (ip : S.inst_pattern) =
    match ip.data with
    | IName(n, _) -> n
  in
  let pos_of (ip : S.inst_pattern) = ip.pos in
  check_uniqueness ~cat:InstPattern ~name_of ~pos_of ips
