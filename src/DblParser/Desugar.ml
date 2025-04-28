(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The first phase of desugaring and post-parsing *)

open Lang.Surface

(** Translation of the binary operator's name to the regular identifier. *)
let tr_bop_id (op : string node) =
  if List.mem op.data ["&&"; "||"; ";"] then
    Error.fatal (Error.reserved_binop_error op.pos op.data)
  else "(" ^ op.data ^ ")"

(** Translation of the unary operator's name to the regular identifier. *)
let make_uop_id str = "(" ^ str ^ " .)"

let tr_bop_to_expr (op : string node) = 
  let make data = { op with data = data } in
  make (EPoly (make (EVar (make (NPName (tr_bop_id op)))), []))

let tr_uop_to_expr (op : string node) = 
  let make data = { op with data = data } in
  make (EPoly (make (EVar (make (NPName (make_uop_id op.data)))), []))

let tr_ctor_name (cname : Raw.ctor_name node) =
  match cname.data with
  | CNUnit   -> "()"
  | CNNil    -> "[]"
  | CNId  c  -> c
  | CNBOp op -> tr_bop_id { cname with data = op }
  | CNUOp op -> make_uop_id op

let tr_ctor_name' (cname : Raw.ctor_name) =
  tr_ctor_name { pos = Position.nowhere; data = cname }

let with_nowhere data = { pos = Position.nowhere; data = data}

let rec node_is_rec_data (def : Raw.def) =
  match def.data with
  | DRecord _ -> true
  | DData   _ -> true
  | DLabel  _ -> true
  | _ -> false

let node_is_data_def (def : def) =
  match def.data with
  | DData  _ -> true
  | DLabel _ -> true
  | _ -> false

let annot_tp e tp =
  { pos = e.pos;
    data = Raw.EAnnot(e, with_nowhere tp)
  }

let scheme_wildcard pos =
  { sch_pos  = pos;
    sch_args = [];
    sch_body = { pos; data = TWildcard }
  }

module RawTypes = struct
  let unit = Raw.TVar(with_nowhere (NPName "Unit"), None)
  let bool = Raw.TVar(with_nowhere (NPName "Bool"), None)
  let string = Raw.TVar(with_nowhere (NPName "String"), None)
end

type ty_def =
  | TD_Id of tvar * Raw.type_expr list
    (** Name with parameters *)

type let_pattern =
  | LP_Id of ident
    (** identifier *)

  | LP_Fun of ident * Raw.expr list
    (** Function definition with list of formal type, named, and explicit
      parameters *)

  | LP_Pat of pattern
    (** Let definition with pattern-matching *)

(** Apply function [f] to each element of [xs]. Function [f] returns elements
  of [Either.t] type, that describes on which list the result should be put.
  It warns, when elements of the right list appear before some element of the
  left list using [warn] function. *)
let rec map_either ~warn f xs =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
    begin match f x with
    | Either.Left y ->
      let (ys, zs) = map_either ~warn f xs in
      (y :: ys, zs)
    | Either.Right z ->
      let (ys, zs) = map_either ~warn f xs in
      if not (List.is_empty ys) then
        Error.warn (warn x.pos);
      (ys, z :: zs)
    end

let map_h_clauses f xs =
  map_either ~warn:Error.finally_before_return_clause f xs

let ident_of_name (name : Raw.name) =
  match name with
  | NVar x | NOptionalVar x -> IdVar x
  | NImplicit n -> IdImplicit n
  | NMethod   n -> IdMethod n

let rec path_append prefix rest =
  let make data = { pos = Position.join prefix.pos rest.pos; data } in
  match rest.data with
  | NPName name       -> make (NPSel(prefix, name))
  | NPSel(rest, name) -> make (NPSel(path_append prefix rest, name))

let rec tr_type_expr (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TWildcard  -> make TWildcard
  | TParen tp  -> make (tr_type_expr tp).data
  | TVar(x, _) -> make (TVar x)
  | TArrow(tp1, tp2) ->
    let sch = tr_scheme_expr tp1 in
    begin match tr_eff_type tp2 with
    | (None, tp2) -> make (TPureArrow(sch, tp2))
    | (Some eff, tp2) -> make (TArrow(sch, tp2, eff))
    end
  | TEffect tps ->
    make (TEffect (List.map tr_type_expr tps))
  | TApp(tp1, tp2) ->
    make (TApp(tr_type_expr tp1, tr_type_expr tp2))
  | TRecord _ | TTypeLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_eff_type (tp : Raw.type_expr) =
  match try_detach_eff tp with
  | None -> (None, tr_type_expr tp)
  | Some(eff, tp) -> (Some (tr_type_expr eff), tr_type_expr tp)

and try_detach_eff (tp : Raw.type_expr) = 
  let make l_pos data = { pos = Position.join l_pos tp.pos; data } in
  match tp.data with
  | TApp({ data = TEffect _; _ } as eff, tp) ->
    Some (eff, tp)
  | TApp(tp1, tp2) ->
    begin match try_detach_eff tp1 with
    | None -> None
    | Some(eff, tp1) -> Some(eff, make tp1.pos (Raw.TApp(tp1, tp2)))
    end
  | TArrow(tp1, tp2) ->
    begin match try_detach_eff tp1 with
    | None -> None
    | Some(eff, tp1) -> Some(eff, make tp1.pos (Raw.TArrow(tp1, tp2)))
    end
  | _ -> None

and tr_scheme_expr (tp : Raw.type_expr) =
  let pos = tp.pos in
  match tp.data with
  | TParen tp ->
    { (tr_scheme_expr tp) with sch_pos = pos }
  | TArrow({ data = TRecord flds; _}, tp) ->
    let args = List.map tr_scheme_field flds in
    begin match tr_eff_type tp with
    | (None, tp) ->
      { sch_pos  = pos;
        sch_args = args;
        sch_body = tp
      }
    | (Some _, _) ->
      Error.fatal (Error.impure_scheme pos)
    end

  | TWildcard | TVar _ | TArrow _ | TEffect _ | TApp _ ->
    { sch_pos  = pos;
      sch_args = [];
      sch_body = tr_type_expr tp
    }
  | TRecord _ | TTypeLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_scheme_field (fld : Raw.ty_field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType tp ->
    let (x, k) = tr_type_var tp in
    make (SA_Type(TNAnon, x, k))
  | FldType(x, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    make (SA_Type(TNVar x, x, k))
  | FldTypeVal(x, arg) ->
    let (y, k) = tr_type_var arg in
    make (SA_Type(TNVar x, y, k))
  | FldName n ->
    make (SA_Val(n, scheme_wildcard fld.pos))
  | FldNameVal(n, tp) ->
    make (SA_Val(n, tr_scheme_expr tp))
  | FldNameAnnot _ | FldModule _ | FldOpen ->
    assert false

(** Translate a type expression as a type variable with kind annotation *)
and tr_type_var (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp -> tr_type_var tp
  | TVar ({data = NPName x; _}, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    (x, k)
  | TVar ({data = NPSel _; _}, _) | TWildcard | TArrow _ | TEffect _ | TApp _
  | TRecord _ | TTypeLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a type expression as a type parameter *)
and tr_type_arg (tp : Raw.type_expr) =
  let (x, k) = tr_type_var tp in
  { tp with data = TA_Var(x, k) }

(** Translate an optional type expression as a type parameter, defaulting to
    wildcard with the supplied position if [None] *)
let tr_type_arg_opt pos (tp : Raw.type_expr option) =
  match tp with
  | Some tp -> tr_type_arg tp
  | None    -> { pos; data = TA_Wildcard }

(** Translate a type expression as a named type parameter *)
let rec tr_named_type_arg (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp -> make (tr_named_type_arg tp).data
  | TVar ({data = NPName x; _}, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    make (TNVar x, make (TA_Var(x, k)))
  | TTypeLbl tp -> make (TNAnon, tr_type_arg tp)
  | TWildcard -> make (TNAnon, make (TA_Wildcard))
  | TVar ({data = NPSel _; _}, _) | TArrow _ | TEffect _ | TApp _
  | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a left-hand-side of the type definition. The additional
  parameter is an accumulated list of formal parameters *)
let rec tr_type_def (tp : Raw.type_expr) args =
  match tp.data with
  | TVar ({data = NPName x; _}, _) -> TD_Id(x, args)
  | TApp(tp1, tp2) -> tr_type_def tp1 (tp2 :: args)
  | TVar ({data = NPSel _; _}, _) | TWildcard | TParen _ | TArrow _
  | TEffect _ | TRecord _ | TTypeLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

let tr_ctor_decl (d : Raw.ctor_decl) =
  let make data = { d with data = data } in
  match d.data with
  | CtorDecl(cd_name, { data = TRecord flds; _ } :: schs ) ->
    let cd_name = tr_ctor_name (make cd_name) in
    let cd_named_args = List.map tr_scheme_field flds in
    let cd_arg_schemes = List.map tr_scheme_expr schs in
    make { cd_name; cd_named_args; cd_arg_schemes }
  | CtorDecl(cd_name, schs) ->
    let cd_name = tr_ctor_name (make cd_name) in
    let cd_arg_schemes = List.map tr_scheme_expr schs in
    make { cd_name; cd_named_args = []; cd_arg_schemes }

(* ========================================================================= *)

(** collect fields of records from the prefix of given list of expressions.
  Returns collected fields, position of the last record-like construct
  (or accumulator [ppos] if there is no records in the prefix), and the rest
  of the expression list *)
let rec collect_fields ~ppos (es : Raw.expr list) =
  match es with
  | [] -> ([], ppos, [])
  | { data = ERecord flds; pos } :: es ->
    let (flds', pos, es) = collect_fields ~ppos:pos es in
    (flds @ flds', pos, es)
  | _ -> ([], ppos, es)

(** Translate a constructor name in a pattern *)
let rec tr_ctor_pattern (p : Raw.expr) =
  let make data = { p with data = data } in
  match p.data with
  | EUnit            -> make (NPName (tr_ctor_name (make Raw.CNUnit)))
  | ECtor c          -> make (NPName c)
  | EBOpID name      -> make (NPName (tr_bop_id (make name)))
  | EUOpID name      -> make (NPName (make_uop_id name))
  | EList []         -> make (NPName (tr_ctor_name (make Raw.CNNil)))
  | ESelect(path, p) -> path_append path (tr_ctor_pattern p)

  | EWildcard | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _ | EVar _
  | EImplicit _ | EFn _ | EApp _ | EDefs _ | EMatch _ | EHandler _ | EEffect _
  | ERecord _ | EMethod _ | EExtern _ | EAnnot _ | EIf _ | EBOp _ | EUOp _
  | EList (_ :: _) | EPub _ | EMethodCall _ | EInterp (_, _) ->
    Error.fatal (Error.desugar_error p.pos)

(** Translate a pattern *)
let rec tr_pattern ~public (p : Raw.expr) =
  let make data = { p with data = data } in
  match p.data with
  | EWildcard   -> make PWildcard
  | EUnit | ECtor _ | ESelect _ ->
    make (PCtor(tr_ctor_pattern p, [], []))
  | ENum _      -> Error.fatal (Error.desugar_error p.pos)
  | ENum64 _      -> Error.fatal (Error.desugar_error p.pos)
  | EStr _      -> Error.fatal (Error.desugar_error p.pos)
  | EChr _      -> Error.fatal (Error.desugar_error p.pos)
  | EInterp _   -> Error.fatal (Error.desugar_error p.pos)
  | EParen    p -> make (tr_pattern ~public p).data
  | EVar      x -> make (PId(public, IdVar x))
  | EBOpID    n -> make (PId(public, IdVar (tr_bop_id (make n))))
  | EUOpID    n -> make (PId(public, IdVar (make_uop_id n)))
  | EImplicit n -> make (PId(public, IdImplicit n))
  | EApp(p1, ps) ->
    let cpath = tr_ctor_pattern p1 in
    let (flds, _, ps) = collect_fields ~ppos:p1.pos ps in
    let named = List.map (tr_named_pattern ~public) flds in
    let ps = List.map (tr_pattern ~public) ps in
    make (PCtor(cpath, named, ps))
  | EAnnot(p, sch) -> make (PAnnot(tr_pattern ~public p, tr_scheme_expr sch))
  | EBOp(p1, op, p2) ->
    let c_name = {op with data = NPName (tr_bop_id op)} in
    let ps = [tr_pattern ~public p1; tr_pattern ~public p2] in
    make (PCtor(c_name, [], ps))
  | EUOp(op, p1) ->
    let c_name = {op with data = NPName (make_uop_id op.data)} in
    make (PCtor(c_name, [], [tr_pattern ~public p1]))
  | EList ps ->
    let cons pe xs =
      let pe  = tr_pattern ~public pe in
      let pos = Position.join pe.pos p.pos in
      let cpath = make (NPName (tr_ctor_name' (CNBOp "::"))) in
      { pos; data = PCtor(cpath, [], [pe; xs]) }
    in
    let nil_path = make (NPName (tr_ctor_name' CNNil)) in
    let pnil = make (PCtor(nil_path, [], [])) in
    make (List.fold_right cons ps pnil).data
  | EPub p -> make (tr_pattern ~public:true p).data

  | EFn _ | EDefs _ | EMatch _ | EHandler _ | EEffect _ | ERecord _
  | EMethod _ | EExtern _ | EIf _ | EMethodCall _  ->
    Error.fatal (Error.desugar_error p.pos)

(** Translate a pattern, separating out its annotation [Some sch] if present
    or returning [None] otherwise. *)
and tr_annot_pattern ~public (p : Raw.expr) =
  let pos = p.pos in
  match p.data with
  | EParen p       ->
    begin match tr_annot_pattern ~public p with
    | pat, None -> { pat with pos }, None
    | (_, Some _) as res -> res
    end
  | EAnnot(p, sch) -> tr_pattern ~public p, Some (tr_scheme_expr sch)
  | EWildcard | EUnit | EVar _ | EBOpID _ | EUOpID _ | EImplicit _ | ECtor _
  | ENum _ | ENum64 _ | EStr _ |  EChr _ | EFn _ | EApp _ | EDefs _ | EMatch _
  | EHandler _ | EEffect _ | ERecord _ | EMethod _ | EMethodCall _ | EExtern _
  | EIf _ | ESelect _ | EBOp _ | EUOp _ | EList _ | EPub _ | EInterp (_, _) ->
    tr_pattern ~public p, None

and tr_named_pattern ~public (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType arg ->
    make (NP_Type(false, make (TNAnon, tr_type_arg arg)))
  | FldType(x, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    make (NP_Type(public, make (TNVar x, make (TA_Var(x, k)))))
  | FldTypeVal(x, arg) ->
    make (NP_Type(public, make (TNVar x, tr_type_arg arg)))
  | FldName n ->
    make (NP_Val(n, make (PId(public, ident_of_name n)), None))
  | FldNameVal(n, p) ->
    let (p, sch) = tr_annot_pattern ~public p in
    make (NP_Val(n, p, sch))
  | FldNameAnnot(n, sch) ->
    let p = make (PId(public, ident_of_name n)) in
    make (NP_Val(n, p, Some (tr_scheme_expr sch)))
  | FldModule { data = NPName name; _ } -> make (NP_Module(public, name))
  | FldModule _ -> Error.fatal (Error.desugar_error fld.pos)
  | FldOpen     -> make (NP_Open public)

(** Translate a parameter declaration *)
let tr_param_decl (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType arg ->
    let (x, k) = tr_type_var arg in
    Either.Left (TNAnon, x, k)
  | FldType(x, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    Either.Left (TNVar x, x, k)
  | FldName n ->
    Either.Right (n, ident_of_name n, None)
  | FldNameAnnot(n, sch) ->
    Either.Right (n, ident_of_name n, Some (tr_scheme_expr sch))

  | FldTypeVal _ | FldNameVal _ | FldModule _ | FldOpen ->
    Error.fatal (Error.desugar_error fld.pos)

(** Translate an expression as a let-pattern. *)
let rec tr_let_pattern ~public (p : Raw.expr) =
  let make data = { p with data = data } in
  match p.data with
  | EVar      x -> LP_Id(IdVar x)
  | EImplicit n -> LP_Id(IdImplicit n)
  | EBOpID    n -> LP_Id(IdVar (tr_bop_id (make n)))
  | EUOpID    n -> LP_Id(IdVar (make_uop_id n))
  | EApp(p1, ps) ->
    begin match p1.data with
    | EVar _ | EImplicit _  | EBOpID _ | EUOpID _->
      let id =
        match p1.data with
        | EVar      x -> IdVar x
        | EImplicit n -> IdImplicit n
        | EBOpID    n -> IdVar (tr_bop_id (make n))
        | EUOpID    n -> IdVar (make_uop_id n)
        | _ -> assert false
      in
      LP_Fun(id, ps)

    | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | ECtor _ | ESelect _
    | EInterp (_, _) | EList _ ->
      LP_Pat(tr_pattern ~public p)

    | EWildcard | EParen _ | EFn _ | EApp _ | EDefs _ 
    | EMatch _ | EHandler _| EEffect _ | ERecord _ | EMethod _ 
    | EExtern _ | EAnnot _ | EIf _ | EBOp _ | EUOp _ | EPub _ | EMethodCall _ ->
      Error.fatal (Error.desugar_error p1.pos)
    end

  | EWildcard | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _
  | ECtor _ | EAnnot _ | EBOp _  | EUOp _  | ESelect _ | EList _ | EPub _ 
  | EInterp (_, _) ->
    LP_Pat (tr_pattern ~public p)

  | EFn _ | EDefs _ | EMatch _ | EHandler _ | EEffect _ | ERecord _
  | EMethod _ | EExtern _ | EIf _ | EMethodCall _  ->
    Error.fatal (Error.desugar_error p.pos)

(** Translate a function, given a list of formal parameters *)
let rec tr_function args body =
  match args with
  | [] -> body
  | arg :: args ->
    { pos  = Position.join arg.pos body.pos;
      data = EFn(tr_pattern ~public:false arg, tr_function args body)
    }

(** Translate a polymorphic function *)
let rec tr_poly_function ~pos all_args body =
  let (flds, _, args) = collect_fields ~ppos:Position.nowhere all_args in
  let named = List.map (tr_named_pattern ~public:false) flds in
  let body = tr_function args body in
  match named with
  | []     -> { pos; data = PE_Expr { body with pos } }
  | _ :: _ -> { pos; data = PE_Fn(named, body) }

(* ========================================================================= *)

let tr_data_vis ?(public=false) (pos : Position.t) (vis : Raw.data_vis) =
  match vis with
  | DV_Private  -> (public, public)
  | DV_Abstract ->
    if public then
      Error.warn (Error.abstr_data_in_pub_block pos);
    (true, public)
  | DV_Public   -> (true, true)

(* ========================================================================= *)

let rec tr_poly_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit       -> make (EVar      (make (NPName (tr_ctor_name' CNUnit))))
  | EVar      x -> make (EVar      (make (NPName x)))
  | EImplicit n -> make (EImplicit (make (NPName n)))
  | ECtor     c -> make (EVar      (make (NPName c)))
  | EBOpID    x -> make (EVar      (make (NPName (tr_bop_id (make x)))))
  | EUOpID    x -> make (EVar      (make (NPName (make_uop_id x))))
  | EList    [] -> make (EVar      (make (NPName (tr_ctor_name' CNNil))))

  | EMethod(e, name) ->
    make (EMethod(tr_expr e, name))
  | ESelect(path, e) ->
    let prepend_path n = path_append path { e with data = NPName n } in
    begin match e.data with
    | EUnit       -> make (EVar      (prepend_path (tr_ctor_name' CNUnit)))
    | EVar      x -> make (EVar      (prepend_path x))
    | EImplicit n -> make (EImplicit (prepend_path n))
    | ECtor     c -> make (EVar      (prepend_path c))
    | EBOpID    x -> make (EVar      (prepend_path (tr_bop_id (make x))))
    | EUOpID    x -> make (EVar      (prepend_path (make_uop_id x)))
    | EList    [] -> make (EVar      (prepend_path (tr_ctor_name' CNNil)))
    
    | EWildcard | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _ | EFn _
    | EApp _ | EEffect _ | EDefs _ | EMatch _ | ERecord _ | EHandler _
    | EExtern _ | EAnnot _ | EIf _ | EMethod _ | ESelect _ | EBOp _ | EUOp _
    | EList (_ :: _) | EPub _ | EMethodCall _ | EInterp (_, _) ->
      Error.fatal (Error.desugar_error e.pos)
    end

  | EWildcard | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _ | EFn _ | EApp _
  | EEffect _ | EDefs _ | EMatch _ | ERecord _ | EHandler _ | EExtern _
  | EAnnot _ | EIf _ | EBOp _ | EUOp _ | EList (_ :: _) | EPub _
  | EMethodCall _ | EInterp (_, _) ->
    Error.fatal (Error.desugar_error e.pos)

and tr_poly_expr_def (e : Raw.expr) =
  let pos = e.pos in
  let make data = { e with data = data } in
  match e.data with
  | EParen e -> make (tr_poly_expr_def e).data
  | EFn(es, e) -> make (tr_poly_function ~pos es (tr_expr e)).data

  | EUnit | EVar _ | EImplicit _ | ECtor _ | EMethod _ | EBOpID _ | EUOpID _ ->
    make (PE_Poly (tr_poly_expr e))

  | ENum _ | ENum64 _ | EStr _ | EChr _ | EApp _ | EMethodCall _ | EDefs _
  | EMatch _ | EHandler _ | EEffect _ | EExtern _ | EAnnot _ | EIf _
  | ESelect _ | EBOp _ | EUOp _ | EList _ | EWildcard | ERecord _ | EPub _ 
  | EInterp (_, _) ->
    make (PE_Expr (tr_expr e))

and tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EParen e       -> make (tr_expr e).data
  | EUnit | EVar _ | EImplicit _ | ECtor _ | EMethod _ | EBOpID _ | EUOpID _ ->
    make (EPoly(tr_poly_expr e, []))
  | ENum n -> make (ENum n)
  | ENum64 n -> make (ENum64 n)
  | EStr s -> make (EStr s)
  | EChr c -> make (EChr c)
  | EInterp (s, xs) ->
    let tr_format (expr : Raw.expr) (fmt : Raw.expr option) = 
      let format = match fmt with
      | None   -> 
        { pos = expr.pos; data = Raw.ECtor "None" }
      | Some fmt ->  
        { pos = expr.pos
        ; data = Raw.EApp (with_nowhere (Raw.ECtor "Some"), [fmt]) }
        in
      let method_name = { pos = expr.pos; data = "format" } in
      let method_call = Raw.EMethodCall (expr, method_name, [format]) in
        { pos = expr.pos; data = method_call } in
    let tr_string str = with_nowhere (Raw.EStr str) in
    let rec flat_interp = function
      | (expr, fmt, str) :: xs 
        -> tr_format expr fmt :: tr_string str :: flat_interp xs
      | [] -> [] in
    let arg_list = make (Raw.EList (tr_string s :: flat_interp xs)) in
    let expr = make (Raw.EApp (make (Raw.EExtern "dbl_strListCat"), [arg_list]))
    in let annot = annot_tp expr RawTypes.string in 
    tr_expr annot
  | EFn(es, e)     -> make (tr_function es (tr_expr e)).data
  | EApp(e1, es)   ->
    begin match collect_fields ~ppos:e1.pos es with
    | [], _, es -> tr_expr_app (tr_expr e1) es
    | flds, fpos, es ->
      let e1 = tr_poly_expr e1 in
      let inst = List.map tr_explicit_inst flds in
      let e1 =
        { pos  = Position.join e1.pos fpos;
          data = EPoly(e1, inst)
        } in
      tr_expr_app e1 es
    end
  | EMethodCall(e1, name, es) ->
    let pos = Position.join e1.pos name.pos in
    let e1 = { pos; data = Raw.EMethod(e1, name.data) } in
    tr_expr_app (tr_expr e1) es
  | EDefs(defs, e) -> make (EDefs(tr_defs defs, tr_expr e))
  | EMatch(e, cls) -> make (EMatch(tr_expr e, List.map tr_match_clause cls))
  | EHandler(h, hcs) ->
    let e = tr_expr h in
    let (rcs, fcs) = map_h_clauses tr_h_clause hcs in
    make (EHandler(e, rcs, fcs))
  | EEffect { label; args; resumption; body } ->
    let (pos, res) =
      match resumption with
      | None     -> (e.pos, make (PId(false, IdVar("resume"))))
      | Some res -> (Position.join res.pos e.pos, tr_pattern ~public:false res)
    in
    let e = EEffect(Option.map tr_expr label, res, tr_expr body) in
    make (tr_function args { pos; data = e }).data
  | EExtern name -> make (EExtern name)
  | EAnnot(e, tp) -> make (EAnnot(tr_expr e, tr_type_expr tp))
  | EIf(e, e1, e2) ->
    let (e1, e2) =
      match e2 with
      | Some e2 -> (e1, e2)
      | None -> (annot_tp e1 RawTypes.unit, with_nowhere Raw.EUnit)
    in
    let ctrue  = make (PCtor(make (NPName "True"), [], [])) in
    let cfalse = make (PCtor(make (NPName "False"), [], [])) in
    let cl1 = Clause(ctrue, tr_expr e1) in
    let cl2 = Clause(cfalse, tr_expr e2) in
    make (EMatch(tr_expr e, [make cl1; make cl2]))
  | ESelect(path, e) ->
    make (EDefs([ make (DOpen(false, path)) ], tr_expr e))
  | EBOp(exp1,op,exp2) ->
    let exp1' = annot_tp exp1 RawTypes.bool in
    let exp2' = annot_tp exp2 RawTypes.bool in
    let e_true = with_nowhere (Raw.ECtor("True")) in
    let e_false = with_nowhere (Raw.ECtor("False")) in
    begin match op.data with
    | "&&" -> tr_expr (make (Raw.EIf(exp1', exp2', Some e_false)))
    | "||" -> tr_expr (make (Raw.EIf(exp1', e_true, Some exp2')))
    | ";" ->
      let lhs = annot_tp exp1 RawTypes.unit in
      tr_expr (make (Raw.EDefs(
        [make (Raw.DLet(false, make Raw.EWildcard, lhs))],
        exp2
      )))
    | _ ->
      let e1 = tr_poly_expr_def exp1 in
      let e2 = tr_poly_expr_def exp2 in
      make (EApp(
        { pos  = Position.join e1.pos op.pos;
          data = EApp(tr_bop_to_expr op, e1)
        }, e2))
    end
  | EUOp(op,exp) ->
    let e = tr_poly_expr_def exp in
    make (EApp (tr_uop_to_expr op, e))
  | EList es ->
    let mk_ctor name = make (EPoly(make (EVar (make (NPName name))), [])) in
    let cons el xs =
      let el  = tr_poly_expr_def el in
      let pos = Position.join el.pos e.pos in
      let make data = { pos; data } in
      make (EApp(make (EApp(mk_ctor (tr_ctor_name' (CNBOp "::")), el)),
                 make (PE_Expr xs)))
    in
    make (List.fold_right cons es (mk_ctor (tr_ctor_name' CNNil))).data

  | EWildcard | ERecord _ | EPub _ ->
    Error.fatal (Error.desugar_error e.pos)

and tr_expr_app (e : expr) (es : Raw.expr list) =
  match es with
  | [] -> e
  | e1 :: es ->
    let e =
      { pos  = Position.join e.pos e1.pos;
        data = EApp(e, tr_poly_expr_def e1)
      } in
    tr_expr_app e es

and tr_match_clause (cl : Raw.match_clause) =
  let make data = { cl with data = data } in
  match cl.data with
  | Clause(pat, body) ->
    make (Clause(tr_pattern ~public:false pat, tr_expr body))

and tr_explicit_inst (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType _ ->
    Error.fatal (Error.desugar_error fld.pos)
  | FldType(x, None) ->
    make (IType(x, make (TVar (make (NPName x)))))
  | FldTypeVal(x, tp) ->
    make (IType(x, tr_type_expr tp))
  | FldName n ->
    let pe =
      match n with
      | NVar x | NOptionalVar x -> make (EVar (make (NPName x)))
      | NImplicit n  -> make (EImplicit (make (NPName n)))
      | NMethod   n  -> Error.fatal (Error.desugar_error fld.pos)
    in
    make (IVal(n, make (PE_Poly pe)))
  | FldNameVal(n, e) ->
    make (IVal(n, tr_poly_expr_def e))
  | FldModule path -> make (IModule path)
  | FldOpen -> make IOpen
  | FldNameAnnot _ | FldType(_, Some _) ->
    Error.fatal (Error.desugar_error fld.pos)

and tr_def ?(public=false) (def : Raw.def) =
  let pos = def.pos in
  let make data = { def with data = data } in
  match def.data with
  | DLet(pub, p, e) ->
    let public = public || pub in
    [ match tr_let_pattern ~public p with
      | LP_Id id ->
        make (DLetId(public, id, tr_poly_expr_def e))
      | LP_Fun(id, args) ->
        make (DLetId(public, id, tr_poly_function ~pos args (tr_expr e)))
      | LP_Pat p ->
        make (DLetPat(p, tr_expr e))
    ]
  | DMethod(pub, p, e) ->
    let public = public || pub in
    [ match tr_let_pattern ~public p with
      | LP_Id (IdVar x) ->
        make (DLetId(public, IdMethod x, tr_poly_expr_def e))
      | LP_Fun(IdVar x, args) ->
        make (DLetId(public,
          IdMethod x, tr_poly_function ~pos args (tr_expr e)))
      | LP_Id (IdImplicit _ | IdMethod _)
      | LP_Fun((IdImplicit _ | IdMethod _), _)
      | LP_Pat _ ->
        Error.fatal (Error.desugar_error p.pos)
    ]
  | DParam fld ->
    [ match tr_param_decl fld with
      | Either.Left (x, y, k)    -> make (DTypeParam(x, y, k))
      | Either.Right (x, y, sch) -> make (DValParam(x, y, sch))
    ]
  | DRecord (vis, tp, flds) ->
    let (public_tp, public_ctors) = tr_data_vis ~public def.pos vis in
    let cd_named_args = List.map tr_scheme_field flds in
    begin match tr_type_def tp [] with
    | TD_Id(cd_name, args) ->
      let args = List.map tr_named_type_arg args in
      let ctors = [ make { cd_name; cd_named_args; cd_arg_schemes = [] } ] in
      let dd =
        make (DData { public_tp; public_ctors; tvar = cd_name; args; ctors })
      in
      let method_named_args, pattern_gen =
        generate_accessor_method_pattern args cd_name in
      dd :: List.filter_map
        (create_accessor_method
          ~public:public_ctors method_named_args pattern_gen)
        cd_named_args
    end
  | DData(vis, tp, cs) ->
    let (public_tp, public_ctors) = tr_data_vis ~public def.pos vis in
    [ match tr_type_def tp [] with
      | TD_Id(tvar, args) ->
        let args = List.map tr_named_type_arg args in
        let ctors = List.map tr_ctor_decl cs in
        make (DData { public_tp; public_ctors; tvar; args; ctors })
    ]
  | DLabel(pub, pat, eff_opt) ->
    let public = public || pub in
    let pat = tr_pattern ~public pat in
    let eff = tr_type_arg_opt def.pos eff_opt in
    [ make (DLabel (eff, pat)) ]
  | DHandle(pub, pat, eff_opt, body, hcs) ->
    let public = public || pub in
    let pat = tr_pattern ~public pat in
    let eff = tr_type_arg_opt def.pos eff_opt in
    let body = tr_expr body in
    let (rcs, fcs) = map_h_clauses tr_h_clause hcs in
    let body = { body with data = EHandler(body, rcs, fcs) } in
    [ make (DHandlePat(pat, eff, body)) ]
  | DHandleWith(pub, pat, eff_opt, body) ->
    let public = public || pub in
    let pat = tr_pattern ~public pat in
    let eff = tr_type_arg_opt def.pos eff_opt in
    let body = tr_expr body in
    [ make (DHandlePat(pat, eff, body)) ]
  | DModule(pub, x, defs) ->
    let public = public || pub in
    [ make (DModule(public, x, tr_defs defs)) ]
  | DOpen(pub, path) -> 
    let public = public || pub in
    [ make (DOpen(public, path)) ]
  | DRec(pub, defs) when List.for_all node_is_rec_data defs ->
    (* This case is a quick fix to make most record accessors
       not marked impure if they aren't. (Explained #160) *)
    (* TODO: Remove when more robust solution is implemented *)
    let public = public || pub in
    let dds, accessors = tr_defs ~public defs
      |> List.partition node_is_data_def in
    make (DRec dds) :: accessors
  | DRec(pub, defs) ->
    let public = public || pub in
    [ make (DRec (tr_defs ~public defs)) ]

and tr_defs ?(public=false) defs = List.concat_map (tr_def ~public) defs

and tr_pattern_with_fields ~public (pat : Raw.expr) =
  match pat.data with
  | EApp({ data = ERecord flds; _ }, [pat]) ->
    (Some flds, tr_pattern ~public pat)
  | EApp({ data = ERecord flds; _ }, p0 :: pats) ->
    let pat =
      { pos = Position.join p0.pos pat.pos;
        data = Raw.EApp(p0, pats)
      } in
    (Some flds, tr_pattern ~public pat)
  | _ ->
    (None, tr_pattern ~public pat)

and tr_h_clause (hc : Raw.h_clause) =
  let make data = { hc with data = data } in
  match hc.data with
  | HCReturn(pat, body) ->
    Either.Left (make (Clause(tr_pattern ~public:false pat, tr_expr body)))
  | HCFinally(pat, body) ->
    Either.Right (make (Clause(tr_pattern ~public:false pat, tr_expr body)))

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

and create_accessor_method ~public named_type_args pattern_gen scheme =
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
      make (DLetId(public, IdMethod field, make (PE_Expr e)))
    | _ :: _ ->
      let targs =
        List.map
          (fun arg -> { arg with data = NP_Type(false, arg) })
          named_type_args
      in
      make (DLetId(public, IdMethod field, make (PE_Fn(targs, e))))
    end
  | SA_Val((NImplicit _ | NMethod _ | NOptionalVar _), _) ->
    Error.warn (Error.ignored_field_in_record scheme.pos);
    None
  | SA_Type _ ->
    Error.fatal (Error.existential_type_arg_in_record scheme.pos)

let tr_program (p : Raw.program) =
  { p with data = tr_defs p.data }
