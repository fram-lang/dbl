(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(* Attributes resolving *)

open Lang.Surface

let map_node f (n : 'a node) = {pos = n.pos; data = f n.data}

module M = Map.Make(String)

type attribute = string list node
type attributes = attribute list

type attr_resolver = 
  attribute 
    -> attributes 
    -> Lang.Surface.def list 
    -> (attributes * Lang.Surface.def list)

type attr_conf = {
  name      : string;
  is_unique : bool;
  preceeds  : string list;
  conflicts : string list;
  resolver  : attr_resolver
}

(* ===== Public/Abstract ===== *)
let make_visible ~is_abstract args attrs ds = 
  let rec make_vis_pattern (pt : Lang.Surface.pattern) =
    map_node begin function
    | PWildcard -> PWildcard
    | PId (_, ident) -> PId (true, ident)
    | PAnnot (pt, scheme) -> PAnnot (make_vis_pattern pt, scheme)
    | PCtor (pth, xs, ys) -> 
      PCtor 
        (pth, List.map make_vis_named_pattern xs, List.map make_vis_pattern ys)
    end pt
  and make_vis_named_pattern (npt : Lang.Surface.named_pattern) =
    map_node begin function
    | NP_Type (_, nta) -> NP_Type (true, nta)
    | NP_Module (_, name) -> NP_Module (true, name)
    | NP_Open _ -> NP_Open true
    | NP_Val (name, pt, scheme) -> NP_Val (name, make_vis_pattern pt, scheme)
    end npt 
  and make_vis_def def =
    map_node begin function
    | DLetId (_, ident, ped) -> 
      DLetId (true, ident, ped)
    | DLetPat (pattern, expr) -> 
      DLetPat (make_vis_pattern pattern, expr)
    | DHandlePat (pattern, ty, expr) ->
      DHandlePat (make_vis_pattern pattern, ty, expr)
    | DData data -> 
      DData { data with public_tp = true; public_ctors = not is_abstract}
    | DModule (_, v, ds) -> DModule (true, v, ds)
    | DOpen (_, pth) -> DOpen (true, pth)
    | DRec ds -> DRec (List.map make_vis_def ds) 
    | DBlock block -> DBlock (List.map make_vis_def block)
    | other -> other
    end def
  in 
  match args.data with
  | [_] -> (attrs, List.map make_vis_def ds)
  | _ -> Error.fatal (Error.attribute_error args.pos "Too many arguments applied to visibility attribute")

let public_attribute : attr_conf = {
  name      = "#pub";
  is_unique = true;
  preceeds  = [];
  conflicts = ["#abstr"];
  resolver  = make_visible ~is_abstract:false
}

let abstr_attribute : attr_conf = {
  name      = "#abstr";
  is_unique = true;
  preceeds  = [];
  conflicts = ["#pub"];
  resolver  = make_visible ~is_abstract:true
}

(* ===== Test ===== *)

let make_test args attrs defs =
  match args.data with
  | _ :: tags ->
    if DblConfig.test_active tags then
      (attrs, defs)
    else
      ([], [])
  | _ -> Error.fatal (Error.attribute_error args.pos "Test attribute expects only 1 optional parameter")

let test_attribute : attr_conf = {
  name      = "test";
  is_unique = false;
  preceeds  = [];
  conflicts = [];
  resolver  = make_test
}

(* ===== Record ====== *)

let tr_record args attrs (defs : Lang.Surface.def list) =
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
    (attrs, dd :: sels)
  | _ -> Error.fatal (Error.attribute_error args.pos "Record error")

let record_attribute : attr_conf = {
  name      = "#record";
  is_unique = true;
  preceeds  = ["#pub"; "#abstr"];
  conflicts = [];
  resolver  = tr_record
}

let ignore_existential_attribute : attr_conf = {
  name      = "ignoreExistential";
  is_unique = true;
  preceeds  = ["#record"];
  conflicts = [];
  resolver  = fun _ -> failwith "not implemented"
}

let attributes = 
  let xs = [
    public_attribute;
    abstr_attribute;
    record_attribute;
    ignore_existential_attribute;
    test_attribute;
  ] in 
  xs |> List.map (fun e -> (e.name, e))|> M.of_list

let attr_name = function
  | { data=name :: _; _ } -> name
  | _ -> failwith "no name :("

(* Topologically sorts attributes 
   and runs checks on them *)
let parse_attributes (attrs : attributes) =

  (* Reduces multiple occurences of given attribute 
     to a list and puts them on a map *)
  let fold_attrs (attrs : attributes) : attributes M.t =
    let rec iter (acc : attributes M.t) = function
      | [] -> acc
      | attr :: attrs -> 
        let name = attr_name attr in
        let add_elem = function
          | Some xs -> Some (attr :: xs)
          | None    -> Some [attr] in
        let acc' = M.update name add_elem acc in
        iter acc' attrs
    in iter M.empty attrs in

  (* checks if attribute matches decalted configuration *)
  let static_check conf curr_attrs other_attrs =
    (* uniquness check *)
    let _ = match (curr_attrs, conf.is_unique) with
    | [x], true -> ()
    | _, false  -> ()
    | _ -> failwith "not unique" in
    (* conflicts check *)
    List.iter 
      (fun c -> if M.mem c other_attrs then failwith "conflict error") 
      conf.conflicts
  in

  (* topologically sorts given attribute and runs checks *)
  let rec visit key (to_visit, stack) =
    match M.find_opt key to_visit with
    | None -> (to_visit, stack)
    | Some xs ->
      begin match M.find_opt key attributes with
      | None -> failwith "unknown attribute"
      | Some conf ->
        let to_visit = M.remove key to_visit in
        let _ = static_check conf xs to_visit in
        let req = conf.preceeds in
        let (to_visit, stack) = 
          List.fold_right visit req (to_visit, stack) in
        (to_visit, xs @ stack) 
      end in
  
  (* Iteratively sorts all attributes *)
  let rec visit_all (attrs, stack) =
    match M.choose_opt attrs with
    | None -> stack
    | Some (key, _) -> visit_all (visit key (attrs, stack)) in

  visit_all (fold_attrs attrs, [])
  
let tr_attrs (attrs : attributes) (defs : Lang.Surface.def list) = 
  let sorted = parse_attributes attrs in 
  let rec iter attrs defs = 
    match attrs with
    | []      -> defs
    | x :: xs ->
      let name = attr_name x in
      let resolver = (M.find name attributes).resolver in
      let (attrs, defs) = resolver x xs defs in
      iter attrs defs in
  iter sorted defs
