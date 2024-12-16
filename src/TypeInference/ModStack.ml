(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Stack of modules which are currently being defined *)
(*
open Common

module StrMap = Map.Make(String)

type t = Module.t * Module.t list

let toplevel = (Module.toplevel, [])

let find_map f (m, stack) =
  match f m with
  | Some _ as x -> x
  | None        -> List.find_map f stack

let fold_right f (m, stack) acc =
  f m (List.fold_right f stack acc)

let add_var (m, stack) ~public x sch =
  let m, y = Module.add_var m ~public x sch in
  (m, stack), y

let add_method_fn (m, stack) ~public x name =
  (Module.add_method_fn m ~public x name, stack)

let add_tvar (m, stack) ~public name kind =
  let m, x = Module.add_tvar m ~public name kind in
  (m, stack), x

let add_type_alias (m, stack) ~public name tp =
  (Module.add_type_alias m ~public name tp, stack)

let add_implicit (m, stack) ~public name sch on_use =
  let m, x = Module.add_implicit m ~public name sch on_use in
  (m, stack), x

let add_ctor (m, stack) ~public name idx info =
  (Module.add_ctor m ~public name idx info, stack)

let lookup_module_name stack name =
  find_map (fun m -> Module.lookup_module m name) stack

let lookup_path stack lookup_in_mod (p : 'a S.path) =
  match p with
  | NPName x    -> find_map (fun m -> lookup_in_mod m x) stack
  | NPSel(x, p) ->
    Option.bind
      (lookup_module_name stack x)
      (fun m -> Module.lookup_path m lookup_in_mod p)

let lookup_var stack = lookup_path stack Module.lookup_var

let lookup_implicit stack = lookup_path stack Module.lookup_implicit

let lookup_ctor stack = lookup_path stack Module.lookup_ctor

let lookup_tvar stack = lookup_path stack Module.lookup_tvar

let lookup_module stack = lookup_path stack Module.lookup_module

let enter_module (m, stack) = (Module.empty, m :: stack)

let leave_module (old_top, stack) ~public name =
  match stack with
  | []               -> assert false
  | new_top :: stack ->
    let old_top = Module.filter_public old_top in
    (Module.add_module new_top ~public name old_top, stack)

let open_module (top, stack) ~public m =
  (Module.open_module top ~public m, stack)
*)
