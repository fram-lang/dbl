(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Represention of module definitions *)

open Common

module StrMap = Map.Make(String)

type top = (closed, modl) opn

type ('a, _) on_closed =
  | SomeCl : 'a -> ('a, closed) on_closed
  | NoneCl : ('a, (_, _) opn) on_closed

(** Information about an ADT definition *)
type adt_info = {
  adt_proof  : T.poly_expr;
  adt_args   : T.named_tvar list;
  adt_ctors  : T.ctor_decl list;
  adt_type   : T.typ;
  adt_effect : T.effct
}

type type_info =
  | TI_Type      of T.typ
  | TI_Parameter of UID.t

type val_info =
  | VI_Var       of T.var * T.scheme
  | VI_Ctor      of int * adt_info
  | VI_Parameter of UID.t

type _ module_state =
  | M_Closed  : closed module_state
  | M_Top     : top module_state
  | M_Section :
    ('st, 'sc) opn module_state -> (('st, 'sc) opn, sec) opn module_state

type ('a, _) ident_info =
  | Public : 'a -> ('a, closed) ident_info
    (** Definition is public *)

  | Private : 'a -> ('a, top) ident_info
    (** Definition is private *)

  | NoDef  : ('a, ('st, _) opn) ident_info
    (** There is no definition of this symbol. *)

  | Skip   : ('a, 'st) ident_info -> ('a, ('st, _) opn) ident_info
    (** There is no definition at this level, but it might be defined in a
      more outer scope *)

  | Local  : 'a * ('a, 'st) ident_info -> ('a, ('st, _) opn) ident_info
    (** There is a local definition (private or a scope parameter) possibly
      shadowing a more outer definition *)

type 'st t = {
  tvar_map : (type_info, 'st) ident_info StrMap.t;
    (** Information about named type variables *)

  val_map  : (val_info, 'st) ident_info Name.Map.t;
    (** Information about named values *)

  ctor_map : (int * adt_info, 'st) ident_info StrMap.t;
    (** Information about ADT constructors *)

  adt_map  : (adt_info, 'st) ident_info T.TVar.Map.t;
    (** Information about ADT definitions. *)

  mod_map : (closed t, 'st) ident_info StrMap.t;
    (** Information about defined modules *)

  exp_methods : (val_info, 'st) ident_info Name.Map.t;
    (** Information about exported methods. It may contain more method than
      the val_map, as it includes methods of the submodules on which the
      [import_adts_and_methods] has been called. *)

  exp_adts : (adt_info, 'st) ident_info T.TVar.Map.t;
    (** Information about exported ADTs. It may contain more ADTs than the
      adt_map, as it includes ADTs of the submodules on which the
      [import_adts_and_methods] has been called. *)

  pp_module : (PPTree.pp_module, 'st) on_closed;
    (** Pretty-printing information. Make sense only for closed modules *)

  state : 'st module_state
    (** Current state of the module *)
}

let empty =
  { tvar_map     = StrMap.empty;
    val_map      = Name.Map.empty;
    ctor_map     = StrMap.empty;
    adt_map      = T.TVar.Map.empty;
    mod_map      = StrMap.empty;
    exp_methods  = Name.Map.empty;
    exp_adts     = T.TVar.Map.empty;
    pp_module    = NoneCl;
    state        = M_Top
  }

(* ========================================================================= *)

let rec add_to_ident_info :
    type st sc. public:_ ->
      (st, sc) opn module_state ->
      ('a, (st, sc) opn) ident_info -> 'a ->
      ('a, (st, sc) opn) ident_info =
  fun ~public state info x ->
  match state, info, public with
  | M_Top, _,                   true  -> Skip (Public x)
  | M_Top, (Private _ | NoDef), false -> Private x
  | M_Top, Skip (Public y),     false -> Local(x, Public y)
  | M_Top, Local(_, p),         false -> Local(x, p)
  | M_Section st, (Skip info | Local(_, info)), _ ->
      Skip (add_to_ident_info ~public st info x)
  | M_Section st, NoDef, _ ->
      Skip (add_to_ident_info ~public st NoDef x)

let update_add ~public state x info_opt =
  let info = Option.value info_opt ~default:NoDef in
  Some (add_to_ident_info ~public state info x)

let add_type_alias ~public m name tp =
  { m with
    tvar_map =
      StrMap.update name (update_add ~public m.state (TI_Type tp))
        m.tvar_map
  }

let add_val ~public m name x sch =
  { m with
    val_map =
      Name.Map.update name (update_add ~public m.state (VI_Var(x, sch)))
        m.val_map;
    exp_methods =
      match name with
      | NMethod _ ->
        Name.Map.update name
          (update_add ~public m.state (VI_Var(x, sch))) m.exp_methods
      | _ -> m.exp_methods
  }

let add_adt ~public m x info =
  assert (not (T.TVar.Map.mem x m.adt_map));
  { m with
    adt_map  =
      T.TVar.Map.update x (update_add ~public m.state info) m.adt_map;
    exp_adts =
      T.TVar.Map.update x (update_add ~public m.state info) m.exp_adts
  }

let add_ctor ~public m name idx info =
  { m with
    val_map  =
      Name.Map.update (NVar name)
        (update_add ~public m.state (VI_Ctor(idx, info)))
        m.val_map;
    ctor_map =
      StrMap.update name (update_add ~public m.state (idx, info))
        m.ctor_map
  }

let add_module ~public m name modl =
  { m with
    mod_map =
      StrMap.update name (update_add ~public m.state modl) m.mod_map
  }

(* ========================================================================= *)

let update_declare v info_opt =
  match info_opt with
  | None | Some NoDef -> Some (Local(v, NoDef))
  | Some (Skip info | Local(_, info)) -> Some (Local(v, info))

let declare_type (type st) (m : (st, sec) opn t) name :
    (st, sec) opn t * UID.t =
  let (M_Section _) = m.state in
  let uid = UID.fresh () in
  let m =
    { m with
      tvar_map =
        StrMap.update name (update_declare (TI_Parameter uid)) m.tvar_map
    }
  in (m, uid)

let declare_val (type st) (m : (st, sec) opn t) name :
    (st, sec) opn t * UID.t =
  let (M_Section _) = m.state in
  let uid = UID.fresh () in
  let m =
    { m with
      val_map =
        Name.Map.update name (update_declare (VI_Parameter uid)) m.val_map
    }
  in (m, uid)

(* ========================================================================= *)

let merge_ident ~public state _ info_opt
    (opened : (_, closed) ident_info option) =
  match info_opt, opened with
  | None, None -> None
  | Some _, None -> info_opt
  | None, Some (Public x) -> Some (add_to_ident_info ~public state NoDef x)
  | Some info, Some (Public x) ->
    Some (add_to_ident_info ~public state info x)

let open_module ~public m opened =
  let mrg name info1 info2 = merge_ident ~public m.state name info1 info2 in
  { tvar_map     = StrMap.merge mrg m.tvar_map opened.tvar_map;
    val_map      = Name.Map.merge mrg m.val_map opened.val_map;
    ctor_map     = StrMap.merge mrg m.ctor_map opened.ctor_map;
    adt_map      = T.TVar.Map.merge mrg m.adt_map opened.adt_map;
    mod_map      = StrMap.merge mrg m.mod_map opened.mod_map;
    exp_methods  = Name.Map.merge mrg m.exp_methods opened.exp_methods;
    exp_adts     = T.TVar.Map.merge mrg m.exp_adts opened.exp_adts;
    pp_module    = m.pp_module;
    state        = m.state
  }

let import_adts_and_methods ~public m opened =
  let mrg name info1 info2 = merge_ident ~public m.state name info1 info2 in
  (* Imported methods and ADTs are added to the current scope as private. By
      doing so, the set of public identifiers of the module doesn't contain
      things that are not defined in the module itself. *)
  let mrg_exp info1 info2 =
    merge_ident ~public:false m.state info1 info2 in
  { m with
    val_map      = Name.Map.merge mrg_exp m.val_map opened.exp_methods;
    adt_map      = T.TVar.Map.merge mrg_exp m.adt_map opened.exp_adts;
    (* But they are added with correct visibility to the "exp" maps, so they
      can be reexported. *)
    exp_methods  = Name.Map.merge mrg m.exp_methods opened.exp_methods;
    exp_adts     = T.TVar.Map.merge mrg m.exp_adts opened.exp_adts
  }

let close_ident _ info =
  match info with
  | Private _ | NoDef -> None
  | Skip(Public x) | Local(_, Public x) -> Some (Public x)

let leave m pp_module =
  let M_Top = m.state in
  { tvar_map    = StrMap.filter_map close_ident m.tvar_map;
    val_map     = Name.Map.filter_map close_ident m.val_map;
    ctor_map    = StrMap.filter_map close_ident m.ctor_map;
    adt_map     = T.TVar.Map.filter_map close_ident m.adt_map;
    mod_map     = StrMap.filter_map close_ident m.mod_map;
    exp_methods = Name.Map.filter_map close_ident m.exp_methods;
    exp_adts    = T.TVar.Map.filter_map close_ident m.exp_adts;
    pp_module   = SomeCl pp_module;
    state       = M_Closed
  }

let enter_section m =
  let enter info = Skip info in
  { tvar_map    = StrMap.map enter m.tvar_map;
    val_map     = Name.Map.map enter m.val_map;
    ctor_map    = StrMap.map enter m.ctor_map;
    adt_map     = T.TVar.Map.map enter m.adt_map;
    mod_map     = StrMap.map enter m.mod_map;
    exp_methods = Name.Map.map enter m.exp_methods;
    exp_adts    = T.TVar.Map.map enter m.exp_adts;
    pp_module   = NoneCl;
    state       = M_Section m.state
  }

let leave_section m =
  let M_Section st = m.state in
  let leave _ (info : (_, (_, sec) opn) ident_info) =
    match info with
    | Skip info | Local(_, info) -> Some info
    | NoDef -> None
  in
  { tvar_map    = StrMap.filter_map leave m.tvar_map;
    val_map     = Name.Map.filter_map leave m.val_map;
    ctor_map    = StrMap.filter_map leave m.ctor_map;
    adt_map     = T.TVar.Map.filter_map leave m.adt_map;
    mod_map     = StrMap.filter_map leave m.mod_map;
    exp_methods = Name.Map.filter_map leave m.exp_methods;
    exp_adts    = T.TVar.Map.filter_map leave m.exp_adts;
    pp_module   = NoneCl;
    state       = st
  } 

(* ========================================================================= *)

let rec get_ident_info : type st. ('a, st) ident_info -> 'a option =
  function
  | Public x | Private x | Local(x, _) -> Some x
  | NoDef    -> None
  | Skip info -> get_ident_info info

let lookup_tvar m x =
  Option.bind (StrMap.find_opt x m.tvar_map) get_ident_info

let lookup_val m x =
  Option.bind (Name.Map.find_opt x m.val_map) get_ident_info

let lookup_ctor m name =
  Option.bind (StrMap.find_opt name m.ctor_map) get_ident_info

let lookup_adt m x =
  Option.bind (T.TVar.Map.find_opt x m.adt_map) get_ident_info

let lookup_module m name =
  Option.bind (StrMap.find_opt name m.mod_map) get_ident_info

(* ========================================================================= *)

let pp_module m =
  match m.pp_module with
  | SomeCl pp_module -> pp_module
