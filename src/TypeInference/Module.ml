(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Represention of module definitions *)

open Common

module StrMap = Map.Make(String)

type on_use = Position.t -> unit

(** Information about an ADT definition *)
type adt_info = {
  adt_proof  : T.poly_expr;
  adt_args   : T.named_tvar list;
  adt_ctors  : T.ctor_decl list;
  adt_type   : T.typ;
  adt_effect : T.effect
}

type var_info =
  | VI_Var      of T.var * T.scheme
  | VI_Ctor     of int * adt_info
  | VI_MethodFn of S.method_name

type 'a ident_info =
  | Public  of 'a
    (** Definition is public *)

  | Private of 'a
    (** Definition is private *)

  | Both    of { pub : 'a; priv : 'a }
    (** Definition is public and shadowed by a private definition *)

type t ={
  tvar_map : (T.typ * on_use) ident_info StrMap.t;
    (** Information about named type variables *)

  var_map  : (var_info * on_use) ident_info StrMap.t;
    (** Information about regular variable names *)

  implicit_map : (T.var * T.scheme * on_use) ident_info StrMap.t;
    (** Information about named implicits *)

  method_map : (T.var * T.scheme * on_use) ident_info StrMap.t T.TVar.Map.t;
    (** Information about named methods *)

  ctor_map : (int * adt_info) ident_info StrMap.t;
    (** Information about ADT constructors *)

  adt_map  : adt_info ident_info T.TVar.Map.t;
    (** Information about ADT definitions *)

  mod_map : t ident_info StrMap.t;
    (** Information about defined modules *)

  pp_module : PPTree.pp_module option;
    (** Pretty-printing information. Make sense only for closed modules *)
}

let empty =
  { tvar_map     = StrMap.empty;
    var_map      = StrMap.empty;
    implicit_map = StrMap.empty;
    method_map   = T.TVar.Map.empty;
    ctor_map     = StrMap.empty;
    adt_map      = T.TVar.Map.empty;
    mod_map      = StrMap.empty;
    pp_module    = None
  }

(* ========================================================================= *)

let update_ident_info ~public map name data =
  if public then
    StrMap.add name (Public data) map
  else
    match StrMap.find_opt name map with
    | None | Some (Private _) -> StrMap.add name (Private data) map
    | Some (Public pub | Both { pub; _ }) ->
      StrMap.add name (Both { pub; priv = data }) map

let add_type_alias ~public ~on_use m name tp =
  { m with
    tvar_map =
      update_ident_info ~public m.tvar_map name (tp, on_use)
  }

let add_var ~public ~on_use m name x sch =
  { m with
    var_map =
      update_ident_info ~public m.var_map name (VI_Var(x, sch), on_use)
  }

let add_implicit ~public ~on_use m name x sch =
  { m with
    implicit_map =
      update_ident_info ~public m.implicit_map name (x, sch, on_use)
  }

let add_method ~public ~on_use m owner name x sch =
  let tab =
    match T.TVar.Map.find_opt owner m.method_map with
    | None -> StrMap.empty
    | Some tab -> tab
  in
  let tab = update_ident_info ~public tab name (x, sch, on_use) in
  { m with
    method_map = T.TVar.Map.add owner tab m.method_map
  }

let add_method_fn ~public ~on_use m x name =
  { m with
    var_map =
      update_ident_info ~public m.var_map x (VI_MethodFn name, on_use)
  }

let add_adt ~public m x info =
  assert (not (T.TVar.Map.mem x m.adt_map));
  let info = if public then Public info else Private info in
  { m with
    adt_map = T.TVar.Map.add x info m.adt_map
  }

let add_ctor ~public m name idx info =
  { m with
    var_map  =
      update_ident_info ~public m.var_map name (VI_Ctor(idx, info), ignore);
    ctor_map =
      update_ident_info ~public m.ctor_map name (idx, info)
  }

let add_module ~public m name modl =
  { m with
    mod_map = update_ident_info ~public m.mod_map name modl
  }

(* ========================================================================= *)

let import_ident_info ~public _ info opened =
  match public, info, opened with
  | _,     _, (None | Some (Private _)) -> info
  | true,  _, Some (Public x | Both { pub = x; _ }) -> Some (Public x)

  | false,
    (None | Some (Private _)),
    Some (Public x | Both { pub = x; _ }) ->
      Some (Private x)

  | false,
    Some (Public pub | Both { pub; _}),
    Some (Public priv | Both { pub = priv; _ }) ->
      Some (Both { pub; priv })

let import_map ~public map opened =
  StrMap.merge (import_ident_info ~public) map opened

let import_method_map ~public =
  T.TVar.Map.merge
    (fun _ tab1 tab2 ->
      match tab1, tab2 with
      | _,    None      -> tab1
      | None, Some tab2 -> Some (import_map ~public StrMap.empty tab2)
      | Some tab1, Some tab2 ->
        Some (import_map ~public tab1 tab2))

let open_module ~public m opened =
  { tvar_map     = import_map ~public m.tvar_map opened.tvar_map;
    var_map      = import_map ~public m.var_map opened.var_map;
    implicit_map = import_map ~public m.implicit_map opened.implicit_map;
    method_map   = import_method_map ~public m.method_map opened.method_map;
    ctor_map     = import_map ~public m.ctor_map opened.ctor_map;
    adt_map      =
      T.TVar.Map.merge (import_ident_info ~public) m.adt_map opened.adt_map;
    mod_map      = import_map ~public m.mod_map opened.mod_map;
    pp_module    = m.pp_module
  }

let filter_public_ident _ info =
  match info with
  | Public x | Both { pub = x; priv = _ } -> Some (Public x)
  | Private _ -> None

let filter_public map =
  StrMap.filter_map filter_public_ident map

let leave m pp_module =
  { tvar_map     = filter_public m.tvar_map;
    var_map      = filter_public m.var_map;
    implicit_map = filter_public m.implicit_map;
    method_map   = T.TVar.Map.map filter_public m.method_map;
    ctor_map     = filter_public m.ctor_map;
    adt_map      = T.TVar.Map.filter_map filter_public_ident m.adt_map;
    mod_map      = filter_public m.mod_map;
    pp_module    = Some pp_module
  }

(* ========================================================================= *)

let get_ident_info info =
  match info with
  | Public x | Private x | Both { pub = _; priv = x } -> x

let lookup m x =
  Option.map get_ident_info (StrMap.find_opt x m)

let lookup_tvar m x =
  lookup m.tvar_map x

let lookup_var m x =
  lookup m.var_map x

let lookup_implicit m x =
  lookup m.implicit_map x

let lookup_method m owner x =
  match T.TVar.Map.find_opt owner m.method_map with
  | None -> None
  | Some tab -> lookup tab x

let lookup_ctor m name =
  lookup m.ctor_map name

let lookup_adt m x =
  Option.map get_ident_info (T.TVar.Map.find_opt x m.adt_map)

let lookup_module m name =
  lookup m.mod_map name

(* ========================================================================= *)

let pp_module m =
  match m.pp_module with
  | None           -> assert false
  | Some pp_module -> pp_module

let public_names map =
  map
  |> StrMap.bindings
  |> List.filter_map
      (fun (name, info) ->
        match info with
        | Public _ | Both _ -> Some name
        | Private _ -> None)

let public_types m =
  public_names m.tvar_map

let public_vars m =
  public_names m.var_map

let public_implicits m =
  public_names m.implicit_map
