(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Find and parse imported modules *)

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

type import_set = StrSet.t

let import_set_empty = StrSet.empty

(** Produce the string representation of a relative path. *)
let string_of_rel_path (p, n) =
  List.fold_right (fun n' id -> n' ^ "/" ^ id) p n

(** Produce the string representation of an absolute path. *)
let string_of_abs_path p =
  "/" ^ string_of_rel_path p

(** Produce the string representation of a path. *)
let string_of_path (p : Raw.import_path) =
  match p with
  | IPAbsolute(p, n) -> string_of_abs_path (p, n)
  | IPRelative(p, n) -> string_of_rel_path (p, n)

(** For an absolute module path produce a string suitable for use as an
    internal module identifier. *)
let internal_mod_id = string_of_abs_path

(** Find the given file in one of the file system directories in [dirs]. *)
let find_file_in dirs path =
  dirs |> List.find_map (fun dirname ->
    let fname = Filename.concat dirname path in
    if Sys.file_exists fname then Some fname else None)

(** Convert a module path to a (relative) file path to be found within the
    file system. *)
let to_file_path (p, n) =
  List.fold_right Filename.concat p (n ^ Configuration.src_extension)

(** Find the file system path for a given absolute module path. *)
let find_mod_abs (p, n) =
  match p with
  | pref :: p when pref = Configuration.local_mod_prefix ->
    find_file_in !Configuration.local_search_dirs (to_file_path (p, n))
  | [] | _ :: _ ->
    find_file_in !Configuration.lib_search_dirs (to_file_path (p, n))

(** Find a module relative to the module directory path [prefix], and return
    the absolute module path and file system path. *)
let find_mod_rel prefix p =
  let rec loop prefix_r (p, n) =
    let abs_path = (List.rev_append prefix_r p, n) in
    match find_mod_abs abs_path with
    | Some fname -> Some (abs_path, fname)
    | None       ->
      begin match prefix_r with
      | []            -> None
      | _ :: prefix_r -> loop prefix_r (p, n)
      end
  in
  loop (List.rev prefix) p

(** Find the absolute module path and file system path for an import path. *)
let find_mod prefix (p : Raw.import_path) =
  match p with
  | IPAbsolute(p, n) -> find_mod_abs (p, n) |> Option.map (fun f -> (p, n), f)
  | IPRelative(p, n) -> find_mod_rel prefix (p, n)

(** Find and parse imported module relative to the path [prefix] and add it
    to the module graph [mods] with the absolute mod identifier as the map key.
    Each vertex in the map stores a list of definition and the module's
    dependencies (graph edges).

    Previously imported modules can be passed in the [imported] parameter
    to avoid including them twice. These modules will not be added to the
    graph, but edges from other vertices may point to them. *)
let rec parse_import ~imported prefix mods (import : Raw.import) =
  let path, new_name =
    match import.data with
    | IImportAs(path, new_name) -> (path, Some new_name)
    | IImportOpen(path)         -> (path, None)
  in
  let path, fname =
    match find_mod prefix path with
    | Some path_fname -> path_fname
    | None            ->
      Error.fatal (Error.cannot_find_module import.pos (string_of_path path))
  in
  let name = internal_mod_id path in
  let import = { import with data = (name, new_name) } in
  if StrSet.mem name imported || StrMap.mem name mods then (mods, import)
  else
    let new_prefix = fst path in
    let imports, defs = File.parse_defs ~pos:import.pos fname in
    let mods = StrMap.add name ([], defs) mods in
    let mods, imports =
      parse_imports ~imported new_prefix mods imports in
    (StrMap.add name (imports, defs) mods, import)

and parse_imports ~imported prefix =
  List.fold_left_map (parse_import ~imported prefix)

(** Build a graph of all (transitively) imported modules. *)
let collect_imports ~imported =
  parse_imports ~imported [ Configuration.local_mod_prefix ] StrMap.empty

let rec sort_dfs mods name on_path (visited, sorted) =
  (* TODO: Once recursive module files are implemented cycle detection in this
     function will likely become redundant. *)
  if StrSet.mem name on_path then
    Error.fatal (Error.module_dependency_cycle name)
  else if StrSet.mem name visited then (visited, sorted)
  else
    match StrMap.find_opt name mods with
    | Some (imports, defs) ->
      let on_path = StrSet.add name on_path in
      let imports, defs = StrMap.find name mods in
      let int_name { Raw.data = (name, _); _ } = name in
      let visited, sorted =
        List.fold_right
          (fun import -> sort_dfs mods (int_name import) on_path)
          imports (visited, sorted) in
      (StrSet.add name visited, (name, imports, defs) :: sorted)
    | None -> (visited, sorted)

(** Topologically sort a graph of modules. *)
let top_sort mods =
  let _, sorted =
    StrMap.fold
      (fun k _ acc -> sort_dfs mods k StrSet.empty acc)
      mods (StrSet.empty, []) in
  List.rev sorted

(** Prepend a module alias based on the given import directive to the list
    of definitions, or open the module directly if no new name is specified. *)
let add_import import defs =
  let open Lang.Surface in
  let make data = { import with data } in
  let mod_id, new_name = import.data in
  match new_name with
  | Some new_name ->
    make (DModule(false, new_name, [ make (DOpen(true, NPName mod_id)) ]))
    :: defs
  | None ->
    make (DOpen(false, NPName mod_id)) :: defs

let add_imports = List.fold_right add_import

let import_many imported imports =
  let mk_mod_def (n, imports, (d : File.def_list)) =
    let defs = add_imports imports d.data in
    { d with data = Lang.Surface.DModule(false, n, defs) }
  in
  let mods, imports = collect_imports ~imported imports in
  let defs = List.map mk_mod_def (top_sort mods) in
  let defs = defs @ add_imports imports [] in
  let imported = StrSet.add_seq (StrMap.to_seq mods |> Seq.map fst) imported in
  (imported, defs)

let import_one imported import = import_many imported [ import ]

let import_prelude () =
  let import : Raw.import =
    { data = IImportOpen(IPAbsolute([], "Prelude"));
      pos  = Position.nowhere
    } in
  import_one import_set_empty import

let prepend_imports ~use_prelude imports p =
  let open Lang.Surface in
  let make data = { p with data } in
  if use_prelude then
    let imported, defs1 = import_prelude () in
    let _,        defs2 = import_many imported imports in
    make (EDefs(defs1 @ defs2, p))
  else
    let _, defs = import_many import_set_empty imports in
    make (EDefs(defs, p))
