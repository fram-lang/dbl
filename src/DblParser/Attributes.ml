(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* Attributes resolving *)

open Lang.Surface

let run_test = ref false

let map_node f (n : 'a node) = {pos = n.pos; data = f n.data}

(* ===== Public ===== *)
let make_public_all (ds : def_data node list) = 

  let make_public_ident ident = 
    match ident with
    | IdImplicit (_, name) -> IdImplicit (true, name)
    | IdMethod (_, name) -> IdMethod (true, name)
    | IdVar (_, name) -> IdVar (true, name)
    | ident -> ident
  in
  
  let make_ctor_decl_public (n : ctor_decl) =
    {n with data = {n.data with cd_public = true}}
  in
  
  let rec make_public def =
    match def with 
  | DLetId (ident, expr) 
    -> DLetId (make_public_ident ident, expr)
  | DLetFun (ident, nts, ns, expr) 
    -> DLetFun (make_public_ident ident, nts, ns, expr)
  | DMethodFn (_, v1, v2) -> DMethodFn (true, v1, v2)
  | DData (_, v, ta, cd) 
    -> DData (true, v, ta, List.map make_ctor_decl_public cd)
  | DModule (_, v, ds) -> DModule (true, v, ds)
  | DOpen (_, pth) -> DOpen (true, pth)
  | DRec ds -> DRec (List.map (map_node make_public) ds) 
  | other -> other
  in 
  
  List.map (map_node make_public) ds

(* ===== Abstract ===== *)

let make_abstract_all (ds : def_data node list) =
  ds

(* ===== Test ===== *)

let make_test defs =
  if !run_test then
    defs
  else 
    []

module M = Map.Make(String)

let attrs : (Lang.Surface.def list -> Lang.Surface.def list) M.t = 
  M.of_list
    [ ("pub", make_public_all)
    ; ("abstr", make_abstract_all)
    ; ("test",   make_test)
    ]

let tr_attr (args : string list node) (data : Lang.Surface.def list) = 
  let f = M.find (List.hd args.data) attrs in
  f data

let tr_attrs (args : string list node list) (data : Lang.Surface.def list) = 
  List.fold_right (fun atr defs -> tr_attr atr defs) args data
    
