(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* Attributes resolving *)

open Lang.Surface

let run_test = ref false

type defs = def list

let mapNode f (n : 'a node) = {pos = n.pos; data = f n.data}

let make_public_ident = function
  | IdImplicit (_, name) -> IdImplicit (true, name)
  | IdMethod (_, name) -> IdMethod (true, name)
  | IdVar (_, name) -> IdVar (true, name)
  | IdLabel -> IdLabel

let make_ctor_decl_public (n : ctor_decl) =
  {n with data = {n.data with cd_public = true}}

let rec make_public = function
  | DLetId (ident, expr) -> DLetId (make_public_ident ident, expr)
  | DLetFun (ident, nts, ns, expr) -> DLetFun (make_public_ident ident, nts, ns, expr)
  | DMethodFn (_, v1, v2) -> DMethodFn (true, v1, v2)
  | DData (_, v, ta, cd) -> DData (true, v, ta, List.map make_ctor_decl_public cd)
  | DModule (_, v, ds) -> DModule (true, v, List.map (mapNode make_public) ds)
  | DOpen (_, pth) -> DOpen (true, pth)
  | DRec ds -> DRec (List.map (mapNode make_public) ds) 
  | other -> other

let make_public_all (ds : def_data node list) = 
  List.map (mapNode make_public) ds

let make_test (defs : defs) =
  if !run_test then
    defs
  else 
    []

module M = Map.Make(String)

let attrs : (defs -> defs) M.t = 
  M.of_list
    [ ("pub", make_public_all)
    ; ("abstr", fun x -> x)
    ; ("test",   make_test)
    ]

let tr_attr (args : string list) (data : defs) = 
  let f = M.find (List.hd args) attrs in
  f data

let tr_attrs (args : string list list) (data : defs) = 
  List.fold_right (fun ats defs -> tr_attr ats defs) args data
    
