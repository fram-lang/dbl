(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* Attributes resolving *)

open Lang.Surface

type defs = def_data node list

let make_public = function
  | DMethodFn (_, v1, v2) -> DMethodFn (true, v1, v2)
  | DData (_, v, ta, cd) -> DData (true, v, ta, cd)
  | DModule (_, v, ds) -> DModule (true, v, ds)
  | DOpen (_, pth) -> DOpen (true, pth)
  | other -> other

let make_public_all (ds : def_data node list) = 
  let mapNode f (n : 'a node) = {pos = n.pos; data = f n.data} in
  List.map (mapNode make_public) ds

let make_test enable (defs : defs) =
  if enable then
    defs
  else 
    []

module M = Map.Make(String)

let attrs : (defs -> defs) M.t = 
  M.of_list
    [ ("public", make_public_all)
    ; ("test",   make_test true)
    ]


