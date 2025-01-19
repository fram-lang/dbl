(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Trees of modules for pretty-printing of types. *)

module StrSet = Set.Make(String)

type pp_module = module_member list
and module_member =
  | MModule of string * pp_module
  | MType   of string * UID.t

type is_public = bool
type visibility = Public | Private | Declared

type member =
  | ModuleMarker
    (** Marker of a new module. *)

  | SectionMarker
    (** Marker of the beginning of a new section. *)

  | Module of is_public * string * pp_module
    (** A module. *)

  | Type of visibility * string * UID.t
    (** A type definition. *)

type var_info =
  | IAnon  of string option * Position.t option
  | INamed of string * Position.t option

type t = {
  env_stack : member list;
    (** Environment represented as a stack of definitions. *)

  var_map   : var_info UID.Map.t
    (** Additional information about variables. *)
}

type pp_result =
  | Found   of string
  | Anon    of string * Position.t option
  | Unbound of string

let empty =
  { env_stack = [];
    var_map   = UID.Map.empty
  }

(* ========================================================================= *)

(** Lookup a module in a module *)
let m_lookup_module name modl =
  modl |> List.find_map (function
    | MModule(name', modl') when name = name' -> Some modl'
    | _ -> None)

(** Lookup a type in a module *)
let m_lookup_type name modl =
  modl |> List.find_map (function
    | MType(name', uid) when name = name' -> Some uid
    | _ -> None)

(* ========================================================================= *)

let merge_opt opt1 opt2 =
  match opt1 with
  | None   -> opt2
  | Some _ -> opt1

let add_named ~vis ?pos pp_tree name uid =
  let update info_opt =
    match info_opt with
    | None                     -> Some (INamed (name, pos))
    | Some (IAnon(_, old_pos)) -> Some (INamed (name, merge_opt pos old_pos))
    | Some (INamed _)          -> info_opt
  in
  { env_stack = Type(vis, name, uid) :: pp_tree.env_stack;
    var_map   = UID.Map.update uid update pp_tree.var_map
  }

let add ~public ?pos pp_tree name uid =
  let vis = if public then Public else Private in
  add_named ~vis ?pos pp_tree name uid
  
let add_anon ?pos ?name pp_tree uid =
  let update info_opt =
    match info_opt with
    | None -> Some (IAnon (name, pos))
    | Some (IAnon(old_name, old_pos)) ->
      Some (IAnon (merge_opt old_name name, merge_opt old_pos pos))
    | Some (INamed _) -> info_opt
  in
  { env_stack = pp_tree.env_stack;
    var_map   = UID.Map.update uid update pp_tree.var_map
  }

let declare ?pos pp_tree name uid =
  add_named ~vis:Declared ?pos pp_tree name uid

let enter_section pp_tree =
  { pp_tree with env_stack = SectionMarker :: pp_tree.env_stack }

let leave_section pp_tree =
  let rec loop acc stack =
    match stack with
    | [] | ModuleMarker :: _ -> assert false
    | SectionMarker :: stack -> List.rev_append acc stack
    | (Module _ | Type((Public | Private), _, _)) as m :: stack ->
      loop (m :: acc) stack
    | Type(Declared, _, _) :: stack -> loop acc stack
  in
  { pp_tree with env_stack = loop [] pp_tree.env_stack }

let enter_module pp_tree =
  { pp_tree with env_stack = ModuleMarker :: pp_tree.env_stack }

let leave_module ~public pp_tree name =
  let add_module name modl acc =
    match m_lookup_module name acc with
    | None   -> MModule (name, modl) :: acc
    | Some _ -> acc
  in
  let add_type name uid acc =
    match m_lookup_type name acc with
    | None   -> MType (name, uid) :: acc
    | Some _ -> acc
  in
  let rec loop acc stack =
    match stack with
    | [] | SectionMarker :: _ -> assert false
    | ModuleMarker :: stack -> (acc, stack)
    | Module(false, _, _) :: stack -> loop acc stack
    | Module(true, name, modl) :: stack ->
      loop (add_module name modl acc) stack
    | Type((Private | Declared), _, _) :: stack -> loop acc stack
    | Type(Public, name, uid) :: stack ->
      loop (add_type name uid acc) stack
  in
  let (modl, stack) = loop [] pp_tree.env_stack in
  let pp_tree =
    { pp_tree with env_stack = Module(public, name, modl) :: stack } in
  (pp_tree, modl)

let open_module ~public pp_tree modl =
  let vis = if public then Public else Private in
  let defs =
    modl |> List.map (function
      | MModule(name, modl') -> Module (public, name, modl')
      | MType(name, uid)     -> Type (vis, name, uid))
  in
  { pp_tree with env_stack = List.rev_append defs pp_tree.env_stack }

(* ========================================================================= *)

let select_path path1 path2 =
  match path1, path2 with
  | None, _ -> path2
  | _, None -> path1
  | Some path1, Some path2 ->
    if List.length path1 <= List.length path2 then Some path1
    else Some path2

let rec lookup_in_module uid modl =
  let rec loop path modl =
    match modl with
    | [] -> path

    | MModule(name, modl') :: modl ->
      let path' = Option.map (List.cons name) (lookup_in_module uid modl') in
      loop (select_path path path') modl

    | MType(name, uid') :: modl when UID.compare uid uid' = 0 ->
      loop (select_path path (Some [name])) modl

    | MType _ :: modl -> loop path modl
  in
  loop None modl

let rec lookup_on_stack uid stack mset tset =
  match stack with
  | [] -> None

  | (ModuleMarker | SectionMarker) :: stack ->
    lookup_on_stack uid stack mset tset

  | Module(_, name, _) :: stack when StrSet.mem name mset ->
    lookup_on_stack uid stack mset tset

  | Module(_, name, modl) :: stack ->
    let mset = StrSet.add name mset in
    let path1 = lookup_on_stack uid stack mset tset in
    let path2 = Option.map (List.cons name) (lookup_in_module uid modl) in
    select_path path1 path2

  | Type(_, name, _) :: stack when StrSet.mem name tset ->
    lookup_on_stack uid stack mset tset

  | Type(_, name, uid') :: stack when UID.compare uid uid' = 0 ->
    let tset = StrSet.add name tset in
    let path = lookup_on_stack uid stack mset tset in
    select_path path (Some [name])

  | Type(_, name, _) :: stack ->
    let tset = StrSet.add name tset in
    lookup_on_stack uid stack mset tset

let lookup pp_tree uid =
  let path = lookup_on_stack uid pp_tree.env_stack StrSet.empty StrSet.empty in
  match path with
  | Some path -> Found (String.concat "." path)
  | None ->
    begin match UID.Map.find_opt uid pp_tree.var_map with
    | Some (IAnon(None, pos))      -> Anon("T", pos)
    | Some (IAnon(Some name, pos)) -> Anon(name, pos)
    | Some (INamed(name, pos))     -> Anon(name, pos)
    | None -> Unbound ("<Unbound" ^ UID.to_string uid ^ ">")
    end
