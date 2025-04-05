(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* Attributes resolving *)

open Lang.Surface

let run_test = ref false

let map_node f (n : 'a node) = {pos = n.pos; data = f n.data}
(*
  kind-expr - OK
  type-expr - Ok
  scheme-expr - OK
  scheme-arg - OK
  kind-arg - OK
  ctor-decl - OK
  type-arg - OK
  named-type-arg - OK

  pattern - DONE
  named-pattern - DONE
  poly_expr_use - DONE
  poly-expr-def - DONE
  expr - DONE
  inst - DONE
  match-clause - DONE
*)
(* ===== Public/Abstract ===== *)
let make_visible (is_abstract : bool) (ds : Lang.Surface.def list) = 
  let rec make_vis_pattern (pt : Lang.Surface.pattern) =
    map_node begin function
    | PWildcard -> PWildcard
    | PId (_, ident) -> PId (true, ident)
    | PAnnot (pt, scheme) -> PAnnot (make_vis_pattern pt, scheme)
    | PCtor (pth, xs, ys) -> 
      PCtor (pth, List.map make_vis_named_pattern xs, List.map make_vis_pattern ys)
    end pt
  and make_vis_named_pattern (npt : Lang.Surface.named_pattern) =
    map_node begin function
    | NP_Type (_, nta) -> NP_Type (true, nta)
    | NP_Module (_, name) -> NP_Module (true, name)
    | NP_Open _ -> NP_Open true
    | NP_Val (name, pt, scheme) -> NP_Val (name, make_vis_pattern pt, scheme)
    end npt 
  and make_vis_match_clause (clause : Lang.Surface.match_clause) =
    map_node begin function
    | Clause (pattern, expr) -> Clause (make_vis_pattern pattern, make_vis_expr expr)
    end clause
  and make_vis_poly_expr_use (peu : Lang.Surface.poly_expr_use) =
    map_node  begin function
    | EMethod (expr, name) -> EMethod (make_vis_expr expr, name)
    | other -> other
    end peu
  and make_vis_poly_expr_def (ped : Lang.Surface.poly_expr_def) =
    map_node begin function
    | PE_Expr expr -> PE_Expr (make_vis_expr expr)
    | PE_Poly peu -> PE_Poly (make_vis_poly_expr_use peu)
    | PE_Fn (nps, expr) -> PE_Fn (List.map make_vis_named_pattern nps, make_vis_expr expr)
    end ped
  and make_vis_inst (inst : Lang.Surface.inst) =
    map_node begin function
    | IVal (name, ped) -> IVal (name, make_vis_poly_expr_def ped)
    | other -> other
    end inst
  and make_vis_expr (expr : Lang.Surface.expr) = 
    map_node begin function
    | EPoly (peu, insts) -> 
      EPoly ( make_vis_poly_expr_use peu
            , List.map make_vis_inst insts)
    | EFn (pattern, expr) -> EFn (make_vis_pattern pattern, make_vis_expr expr)
    | EApp (expr, pe) -> EApp (make_vis_expr expr, make_vis_poly_expr_def pe)
    (* defs ? *)
    | EDefs (defs, expr) -> EDefs (List.map make_vis_def defs, make_vis_expr expr)
    | EMatch (expr, clauses) ->
      EMatch (make_vis_expr expr, List.map make_vis_match_clause clauses)
    | EHandler (expr, cls1, cls2) ->
      EHandler ( make_vis_expr expr
               , List.map make_vis_match_clause cls1
               , List.map make_vis_match_clause cls2)
    | EEffect (oexpr, pattern, expr) ->
      EEffect ( Option.map make_vis_expr oexpr
              , make_vis_pattern pattern
              , make_vis_expr expr)
    | EAnnot (expr, ty) -> EAnnot (make_vis_expr expr, ty)
    (* ? *)
    | ERepl xs -> ERepl (Seq.map (List.map make_vis_def) xs)
    | other -> other
    end expr   
  and make_vis_def def =
    map_node begin function
    | DLetId (_, ident, ped) -> 
      DLetId (true, ident, make_vis_poly_expr_def ped)
    | DLetPat (pattern, expr) -> 
      DLetPat (make_vis_pattern pattern, make_vis_expr expr)
    | DLabel (ty, pattern) -> 
      DLabel (ty, make_vis_pattern pattern)
    | DHandlePat (pattern, ty, expr) ->
      DHandlePat (make_vis_pattern pattern, ty, make_vis_expr expr)
    | DData data -> 
      DData { data with public_tp = true; public_ctors = not is_abstract}
    | DModule (_, v, ds) -> DModule (true, v, ds)
    | DOpen (_, pth) -> DOpen (true, pth)
    | DRec ds -> DRec (List.map make_vis_def ds) 
    | DBlock block -> DBlock (List.map make_vis_def block)
    (* ? *)
    | DReplExpr expr -> DReplExpr (make_vis_expr expr)
    | other -> other
    end def
  in List.map make_vis_def ds

(* ===== Test ===== *)

let make_test defs =
  if !run_test then
    defs
  else 
    []

module M = Map.Make(String)

let attrs : (Lang.Surface.def list -> Lang.Surface.def list) M.t = 
  M.of_list
    [ ("pub", make_visible false)
    ; ("abstr", make_visible true)
    ; ("test",   make_test)
    ]

let tr_attr (args : string list node) (data : Lang.Surface.def list) = 
  let f = M.find (List.hd args.data) attrs in
  f data

let tr_attrs (args : string list node list) (data : Lang.Surface.def list) = 
  List.fold_right (fun atr defs -> tr_attr atr defs) args data
    
