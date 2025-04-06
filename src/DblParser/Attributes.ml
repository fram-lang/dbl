(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* Attributes resolving *)

open Lang.Surface


let map_node f (n : 'a node) = {pos = n.pos; data = f n.data}

(* ===== Public/Abstract ===== *)
let make_visible (is_abstract : bool) (args : string list node) (ds : Lang.Surface.def list) = 
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
  in 
  match args.data with
  | [_] -> List.map make_vis_def ds
  | _ -> Error.fatal (Error.attribute_error args.pos "Too many arguments applied to visibility attribute")

(* ===== Test ===== *)

let make_test (args : string list node) defs =
  match args.data with
  | _ :: tags ->
    if DblConfig.test_active tags then
      defs
    else
      []
  | _ -> Error.fatal (Error.attribute_error args.pos "Test attribute expects only 1 optional parameter")

(* ===== Record ====== *)

let tr_record (args : string list node) (defs : Lang.Surface.def list) =
  let rec create_accessor_method named_type_args pattern_gen scheme =
    match scheme.data with
    | SA_Val(NVar field, _) ->
      let make data = { scheme with data } in
      (* generate accessor body, this piece of code generated accessing
      variable *)
      let e =
        make (EFn(pattern_gen field,
          make (EPoly(make (EVar (make (NPName field))), [])))) in
      Some begin match named_type_args with
      | [] ->
        make (DLetId(false, IdMethod field, make (PE_Expr e)))
      | _ :: _ ->
        let targs =
          List.map
            (fun arg -> { arg with data = NP_Type(false, arg) })
            named_type_args
        in
        make (DLetId(false, IdMethod field, make (PE_Fn(targs, e))))
      end
    | SA_Val((NImplicit _ | NMethod _ | NOptionalVar _), _) ->
      Error.warn (Error.ignored_field_in_record scheme.pos);
      None
    | SA_Type _ ->
      Error.fatal (Error.existential_type_arg_in_record scheme.pos)
  (** Returns: list of explicit type annotations with fresh type variable names
      and a function that given a field name returns piece of code that
      represents pattern which pulls out this variable, making it easy to use in
      generated code *)
  and generate_accessor_method_pattern named_type_args type_name =
    let make data = { data; pos=Position.nowhere } in
    (* generate new names impossible to input by user *)
    let create_mapping i arg =
      let old_name, new_name =
        match (snd arg.data).data with
        | TA_Var(name, _) -> name, name ^ "#TA_Var#" ^ string_of_int i
        | TA_Wildcard -> "TNAnon", "TNAnon#TA_Wildcard#" ^ string_of_int i
      in
      let named_arg =
        make (TNVar old_name, make (TA_Var (new_name, make KWildcard))) in
      named_arg, new_name
    in
    let (new_named_type_args, new_names : named_type_arg list * ctor_name list) =
      List.split (List.mapi create_mapping named_type_args) in
    (* function for fold that generates nested series of tapps for type
      annotation *)
    let gen_tapps inner name =
      make (TApp(inner, make (TVar (make (NPName name))))) in
    let type_annot : scheme_expr =
      { sch_pos = Position.nowhere
      ; sch_args = []
      ; sch_body =
        List.fold_left
          gen_tapps
          (make (TVar (make (NPName type_name))))
          new_names
      } in
      (* function that generates pattern for accessing field *)
      let pattern_gen field =
        make (PAnnot(make (PCtor(
          make (NPName type_name),
          [ make (NP_Val(NVar field, make (PId(false, IdVar field)), None)) ],
          [])), type_annot))
      in
      new_named_type_args, pattern_gen
  in
  match (args.data, defs) with
  | ([_], [{data=DData data; pos=pos}]) ->
    let dd = {pos=pos; data=DData data} in
    let method_named_args, pattern_gen =
      generate_accessor_method_pattern data.args data.tvar in
    let cd_named_args = (List.hd data.ctors).data.cd_named_args in
    let sels = List.filter_map (create_accessor_method method_named_args pattern_gen) cd_named_args in
    dd :: sels
  | _ -> Error.fatal (Error.attribute_error args.pos "Record error")

        module M = Map.Make(String)

type attribute_fun = string list node -> Lang.Surface.def list -> Lang.Surface.def list

let attrs : attribute_fun M.t = 
  M.of_list
    [ ("pub", make_visible false)
    ; ("abstr", make_visible true)
    ; ("test",   make_test)
    ; ("record", tr_record)
    ]

let tr_attr (args : string list node) (data : Lang.Surface.def list) = 
  let f = M.find_opt (List.hd args.data) attrs in
  match f with
  | Some f -> f args data
  | None -> Error.fatal (Error.attribute_error args.pos ("Unknown attribute " ^ List.hd args.data))

let tr_attrs (args : Raw.attributes) (data : Lang.Surface.def list) = 
  List.fold_right (fun atr defs -> tr_attr atr defs) args data

let tr_attrs_ext (args : Raw.attributes) (data : (Raw.attributes * Lang.Surface.def) list) =
  List.concat_map (fun (attrs, data) -> tr_attrs attrs [data]) data
  |> tr_attrs args
