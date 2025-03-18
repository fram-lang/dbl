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
  make (EPoly (make (EVar (NPName (tr_bop_id op))),[],[]))

let tr_uop_to_expr (op : string node) = 
  let make data = { op with data = data } in
  make (EPoly (make (EVar (NPName (make_uop_id op.data))),[],[]))

let tr_var_id (var : Raw.var_id node) =
  match var.data with
  | VIdVar x  -> x
  | VIdBOp op -> tr_bop_id { var with data = op }
  | VIdUOp op -> make_uop_id op

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
  match (snd def).data with
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

module RawTypes = struct
  let unit = Raw.TVar(NPName "Unit", None)
  let bool = Raw.TVar(NPName "Bool", None)
end

type ty_def =
  | TD_Id of tvar * Raw.type_expr list
    (** Name with parameters *)

type let_pattern =
  | LP_Id of ident
    (** identifier *)

  | LP_Fun of ident * named_type_arg list * named_arg list * Raw.expr list
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

let map_inst_like f xs =
  map_either ~warn:Error.value_before_type_param f xs

let map_h_clauses f xs =
  map_either ~warn:Error.finally_before_return_clause f xs

(** Finds argument named "self" on given list. *)
let rec find_self_arg args =
  match args with
  | [] -> (None, [])
  | { pos; data = (NVar "self", arg) } :: args ->
    begin match find_self_arg args with
    | (None, _) -> (Some (pos, arg), args)
    | (Some(pos, _), _) ->
      Error.fatal (Error.multiple_self_parameters pos)
    end
  | arg :: args ->
    let (self, args) = find_self_arg args in
    (self, arg :: args)

let ident_of_name ~public (name : Raw.name) =
  match name with
  | NLabel      -> IdLabel
  | NVar x | NOptionalVar x -> IdVar(public, x)
  | NImplicit n -> IdImplicit(public, n)
  | NMethod   n -> IdMethod(public, n)

let rec path_append path rest =
  match path with
  | NPName name       -> NPSel(name, rest)
  | NPSel(name, path) -> NPSel(name, path_append path rest)

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
  | TEffect(tps, ee) ->
    make (TEffect(List.map tr_type_expr tps, Option.map tr_type_expr ee))
  | TApp(tp1, tp2) ->
    make (TApp(tr_type_expr tp1, tr_type_expr tp2))
  | TRecord _ | TTypeLbl _ | TEffectLbl _ ->
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
    let (tvs, named) = map_inst_like tr_scheme_field flds in
    begin match tr_eff_type tp with
    | (None, tp) ->
      { sch_pos   = pos;
        sch_targs = tvs;
        sch_named = named;
        sch_body  = tp
      }
    | (Some _, _) ->
      Error.fatal (Error.impure_scheme pos)
    end

  | TWildcard | TVar _ | TArrow _ | TEffect _ | TApp _ ->
    { sch_pos   = pos;
      sch_targs = [];
      sch_named = [];
      sch_body  = tr_type_expr tp
    }
  | TRecord _ | TTypeLbl _ | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_scheme_field (fld : Raw.ty_field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType tp ->
    Either.Left (make (TNAnon, tr_type_arg ~public:false tp))
  | FldEffect ->
    Either.Left (make (TNEffect, make TA_Effect))
  | FldEffectVal arg ->
    Either.Left (make (TNEffect, tr_type_arg ~public:false arg))
  | FldType(x, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    Either.Left (make (TNVar x, make (TA_Var(false, x, k))))
  | FldTypeVal(x, arg) ->
    Either.Left (make (TNVar x, tr_type_arg ~public:false arg))
  | FldName n ->
    let sch =
      { sch_pos   = fld.pos;
        sch_targs = [];
        sch_named = [];
        sch_body  = make TWildcard
      }
    in
    Either.Right (make (n, sch))
  | FldNameVal(n, tp) ->
    Either.Right (make (n, tr_scheme_expr tp))
  | FldNameAnnot _ | FldModule _ ->
    assert false

(** Translate a type expression as a type parameter *)
and tr_type_arg ~public (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp -> make (tr_type_arg ~public tp).data
  | TVar (NPName x, ka) -> 
    let k = Option.value ka ~default:(make KWildcard) in
    make (TA_Var(public, x, k))
  | TVar (NPSel _, _) | TWildcard | TArrow _ | TEffect _ | TApp _ | TRecord _
  | TTypeLbl _ | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a type expression as a named type parameter *)
let rec tr_named_type_arg (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp -> make (tr_named_type_arg tp).data
  | TVar (NPName x, ka) -> 
    let k = Option.value ka ~default:(make KWildcard) in
    make (TNVar x, make (TA_Var(false, x, k)))
  | TTypeLbl tp -> make (TNAnon, tr_type_arg ~public:false tp)
  | TEffectLbl tp -> make (TNEffect, tr_type_arg ~public:false tp)
  | TWildcard -> make (TNAnon, make (TA_Wildcard))
  | TVar (NPSel _, _) | TArrow _ | TEffect _ | TApp _ | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a left-hand-side of the type definition. The additional
  parameter is an accumulated list of formal parameters *)
let rec tr_type_def (tp : Raw.type_expr) args =
  match tp.data with
  | TVar (NPName x, _) -> TD_Id(x, args)
  | TApp(tp1, tp2) -> tr_type_def tp1 (tp2 :: args)
  | TVar (NPSel _, _) | TWildcard | TParen _ | TArrow _ | TEffect _ | TRecord _
  | TTypeLbl _ | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

let tr_ctor_decl ~public:cd_public (d : Raw.ctor_decl) =
  let make data = { d with data = data } in
  match d.data with
  | CtorDecl(cd_name, { data = TRecord flds; _ } :: schs ) ->
    let cd_name = tr_ctor_name (make cd_name) in
    let (cd_targs, cd_named) = map_inst_like tr_scheme_field flds in
    let cd_arg_schemes = List.map tr_scheme_expr schs in
    make { cd_public; cd_name; cd_targs; cd_named; cd_arg_schemes }
  | CtorDecl(cd_name, schs) ->
    let cd_name = tr_ctor_name (make cd_name) in
    let cd_arg_schemes = List.map tr_scheme_expr schs in
    make { cd_public; cd_name; cd_targs = []; cd_named = []; cd_arg_schemes }

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
  | EUnit            -> NPName (tr_ctor_name (make Raw.CNUnit))
  | ECtor c          -> NPName c
  | EBOpID name      -> NPName (tr_bop_id { p with data = name})
  | EUOpID name      -> NPName (make_uop_id name)
  | EList []         -> NPName (tr_ctor_name (make Raw.CNNil))
  | ESelect(path, p) -> path_append path (tr_ctor_pattern p)

  | EWildcard | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _ | EVar _
  | EImplicit _ | EFn _ | EApp _ | EDefs _ | EMatch _ | EHandler _ | EEffect _
  | ERecord _ | EMethod _ | EExtern _ | EAnnot _ | EIf _ | EBOp _ | EUOp _
  | EList (_ :: _) | EPub _ | EMethodCall _ ->
    Error.fatal (Error.desugar_error p.pos)

(** Translate a pattern *)
let rec tr_pattern ~public (p : Raw.expr) =
  let make data = { p with data = data } in
  match p.data with
  | EWildcard   -> make PWildcard
  | EUnit | ECtor _ | ESelect _ ->
    make (PCtor(make (tr_ctor_pattern p), CNParams([], []), []))
  | ENum _      -> Error.fatal (Error.desugar_error p.pos)
  | ENum64 _      -> Error.fatal (Error.desugar_error p.pos)
  | EStr _      -> Error.fatal (Error.desugar_error p.pos)
  | EChr _      -> Error.fatal (Error.desugar_error p.pos)
  | EParen    p -> make (tr_pattern ~public p).data
  | EVar      x -> make (PId (IdVar(public, x)))
  | EBOpID    n -> make (PId (IdVar(public, tr_bop_id (make n))))
  | EUOpID    n -> make (PId (IdVar(public, make_uop_id n)))
  | EImplicit n -> make (PId (IdImplicit(public, n)))
  | EApp(p1, ps) ->
    let cpath = { p1 with data = tr_ctor_pattern p1 } in
    let (flds, _, ps) = collect_fields ~ppos:p1.pos ps in
    let ps = List.map (tr_pattern ~public) ps in
    begin match flds with
    | [ { data = FldModule name; _ } ] ->
      make (PCtor(cpath, CNModule(public, name), ps))
    | _ ->
      let (targs, iargs) = map_inst_like (tr_named_pattern ~public) flds in
      make (PCtor(cpath, CNParams(targs, iargs), ps))
    end
  | EAnnot(p, sch) -> make (PAnnot(tr_pattern ~public p, tr_scheme_expr sch))
  | EBOp(p1, op, p2) ->
    let c_name = {op with data = NPName (tr_bop_id op)} in
    let ps = [tr_pattern ~public p1; tr_pattern ~public p2] in
    make (PCtor(c_name, CNParams([], []), ps))
  | EUOp(op, p1) ->
    let c_name = {op with data = NPName (make_uop_id op.data)} in
    make (PCtor(c_name, CNParams([], []), [tr_pattern ~public p1]))
  | EList ps ->
    let cons pe xs =
      let pe  = tr_pattern ~public pe in
      let pos = Position.join pe.pos p.pos in
      let cpath = make (NPName (tr_ctor_name' (CNBOp "::"))) in
      { pos; data = PCtor(cpath, CNParams([], []), [pe; xs]) }
    in
    let nil_path = make (NPName (tr_ctor_name' CNNil)) in
    let pnil = make (PCtor(nil_path, CNParams([], []), [])) in
    make (List.fold_right cons ps pnil).data
  | EPub p -> make (tr_pattern ~public:true p).data

  | EFn _ | EDefs _ | EMatch _ | EHandler _ | EEffect _ | ERecord _
  | EMethod _ | EExtern _ | EIf _ | EMethodCall _  ->
    Error.fatal (Error.desugar_error p.pos)

and tr_named_pattern ~public (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType _ ->
    Error.fatal (Error.anon_type_pattern fld.pos)
  | FldEffect ->
    Either.Left (make (TNEffect, make TA_Effect))
  | FldEffectVal arg ->
    Either.Left (make (TNEffect, tr_type_arg ~public arg))
  | FldType(x, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    Either.Left (make (TNVar x, make (TA_Var(public, x, k))))
  | FldTypeVal(x, arg) ->
    Either.Left (make (TNVar x, tr_type_arg ~public arg))
  | FldName n ->
    Either.Right (make (n, make (PId (ident_of_name ~public n))))
  | FldNameVal(n, p) ->
    Either.Right (make (n, tr_pattern ~public p))
  | FldNameAnnot(n, sch) ->
    Either.Right
      (make (n, make (PAnnot(make (PId (ident_of_name ~public n)),
                             tr_scheme_expr sch))))
  | FldModule _ ->
    Error.fatal (Error.desugar_error fld.pos)

(** Translate a formal parameter of a function *)
let rec tr_function_arg (arg : Raw.expr) =
  match arg.data with
  | EParen arg -> tr_function_arg arg
  | EAnnot(p, sch) ->
    ArgAnnot(tr_pattern ~public:false p, tr_scheme_expr sch)
  | EWildcard | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EVar _
  | EImplicit _ | ECtor _ | EBOp _ | EUOp _ | EApp _ | EBOpID _ | EUOpID _
  | ESelect _ | EList _ ->
    ArgPattern (tr_pattern ~public:false arg)

  | EFn _ | EEffect _ | EDefs _ | EMatch _ | EHandler _ | ERecord _
  | EMethod _ | EExtern _ | EIf _ | EPub _ | EMethodCall _ ->
    Error.fatal (Error.desugar_error arg.pos)

let tr_named_arg (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType arg ->
    Either.Left (make (TNAnon, tr_type_arg ~public:false arg))
  | FldEffect ->
    Either.Left (make (TNEffect, make TA_Effect))
  | FldEffectVal arg ->
    Either.Left (make (TNEffect, tr_type_arg ~public:false arg))
  | FldType(x, ka) ->
    let k = Option.value ka ~default:(make KWildcard) in
    Either.Left (make (TNVar x, make (TA_Var(false, x, k))))
  | FldTypeVal(x, arg) ->
    Either.Left (make (TNVar x, tr_type_arg ~public:false arg))
  | FldName n ->
    let arg = ArgPattern(make (PId (ident_of_name ~public:false n))) in
    Either.Right (make (n, arg))
  | FldNameVal(n, e) ->
    Either.Right (make (n, tr_function_arg e))
  | FldNameAnnot(n, sch) ->
    let arg =
      ArgAnnot(make (PId (ident_of_name ~public:false n)), tr_scheme_expr sch)
    in
    Either.Right (make (n, arg))
  | FldModule _ ->
    (* TODO: This might eventually be supported. *)
    Error.fatal (Error.desugar_error fld.pos)

(** Translate an expression as a let-pattern. *)
let rec tr_let_pattern ~public (p : Raw.expr) =
  let make data = { p with data = data } in
  match p.data with
  | EVar      x -> LP_Id(IdVar(public, x))
  | EImplicit n -> LP_Id(IdImplicit(public, n))
  | EBOpID    n -> LP_Id(IdVar(public, tr_bop_id (make n)))
  | EUOpID    n -> LP_Id(IdVar(public, make_uop_id n))
  | EApp(p1, ps) ->
    begin match p1.data with
    | EVar _ | EImplicit _  | EBOpID _ | EUOpID _->
      let id =
        match p1.data with
        | EVar      x -> IdVar(public, x)
        | EImplicit n -> IdImplicit(public, n)
        | EBOpID    n -> IdVar(public, tr_bop_id (make n))
        | EUOpID    n -> IdVar(public, make_uop_id n)
        | _ -> assert false
      in
      let (flds, _, ps) = collect_fields ~ppos:p1.pos ps in
      let (targs, iargs) = map_inst_like tr_named_arg flds in
      LP_Fun(id, targs, iargs, ps)

    | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | ECtor _ | ESelect _
    | EList _ ->
      LP_Pat(tr_pattern ~public p)

    | EWildcard | EParen _ | EFn _ | EApp _ | EDefs _ 
    | EMatch _ | EHandler _| EEffect _ | ERecord _ | EMethod _ 
    | EExtern _ | EAnnot _ | EIf _ | EBOp _ | EUOp _ | EPub _ | EMethodCall _ ->
      Error.fatal (Error.desugar_error p1.pos)
    end

  | EWildcard | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _
  | ECtor _ | EAnnot _ | EBOp _  | EUOp _  | ESelect _ | EList _ | EPub _ ->
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
      data = EFn(tr_function_arg arg, tr_function args body)
    }

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
  | EUnit       -> make (EVar      (NPName (tr_ctor_name' CNUnit)))
  | EVar      x -> make (EVar      (NPName x))
  | EImplicit n -> make (EImplicit (NPName n))
  | ECtor     c -> make (EVar      (NPName c))
  | EBOpID    x -> make (EVar      (NPName (tr_bop_id (make x))))
  | EUOpID    x -> make (EVar      (NPName (make_uop_id x)))
  | EList    [] -> make (EVar      (NPName (tr_ctor_name' CNNil)))

  | EMethod(e, name) ->
    make (EMethod(tr_expr e, name))
  | ESelect(path, e) ->
    let prepend_path n = path_append path (NPName n) in
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
    | EList (_ :: _) | EPub _ | EMethodCall _ ->
      Error.fatal (Error.desugar_error e.pos)
    end

  | EWildcard | ENum _ | ENum64 _ | EStr _ | EChr _ | EParen _ | EFn _ | EApp _
  | EEffect _ | EDefs _ | EMatch _ | ERecord _ | EHandler _ | EExtern _
  | EAnnot _ | EIf _ | EBOp _ | EUOp _ | EList (_ :: _) | EPub _
  | EMethodCall _ ->
    Error.fatal (Error.desugar_error e.pos)

and tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EParen e       -> make (tr_expr e).data
  | EUnit | EVar _ | EImplicit _ | ECtor _ | EMethod _ | EBOpID _ | EUOpID _ ->
    make (EPoly(tr_poly_expr e, [], []))
  | ENum n -> make (ENum n)
  | ENum64 n -> make (ENum64 n)
  | EStr s -> make (EStr s)
  | EChr c -> make (EChr c)
  | EFn(es, e)     -> make (tr_function es (tr_expr e)).data
  | EApp(e1, es)    ->
    begin match collect_fields ~ppos:e1.pos es with
    | [], _, es -> tr_expr_app (tr_expr e1) es
    | flds, fpos, es ->
      let e1 = tr_poly_expr e1 in
      let (tinst, inst) = map_inst_like tr_explicit_inst flds in
      let e1 =
        { pos  = Position.join e1.pos fpos;
          data = EPoly(e1, tinst, inst)
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
  | EEffect(es, rp_opt, e) ->
    let (pos, rp) =
      match rp_opt with
      | None    -> (e.pos, ArgPattern (make (PId (IdVar(false, "resume")))))
      | Some rp -> (Position.join rp.pos e.pos, tr_function_arg rp)
    in
    make (tr_function es { pos; data = EEffect(rp, tr_expr e)}).data
  | EExtern name -> make (EExtern name)
  | EAnnot(e, tp) -> make (EAnnot(tr_expr e, tr_type_expr tp))
  | EIf(e, e1, e2) ->
    let (e1, e2) =
      match e2 with
      | Some e2 -> (e1, e2)
      | None -> (annot_tp e1 RawTypes.unit, with_nowhere Raw.EUnit)
    in
    let ctrue  = make (PCtor(make (NPName "True"), CNParams([], []), [])) in
    let cfalse = make (PCtor(make (NPName "False"), CNParams([], []), [])) in
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
        [([], make (Raw.DLet(false, make Raw.EWildcard, lhs)))],
        exp2
      )))
    | _ ->
      let e1 = tr_expr exp1 and e2 = tr_expr exp2 in
      make (EApp(
        { pos  = Position.join e1.pos op.pos;
          data = EApp(tr_bop_to_expr op, e1)
        }, e2))
    end
  | EUOp(op,exp) ->
    let e = tr_expr exp in 
    make (EApp (tr_uop_to_expr op, e))
  | EList es ->
    let mk_ctor name = make (EPoly(make (EVar (NPName name)), [], [])) in
    let cons el xs =
      let el  = tr_expr el in
      let pos = Position.join el.pos e.pos in
      let make data = { pos; data } in
      make (EApp(make (EApp(mk_ctor (tr_ctor_name' (CNBOp "::")), el)), xs))
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
        data = EApp(e, tr_expr e1)
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
  | FldEffectVal eff ->
    Either.Left (make (TNEffect, tr_type_expr eff))
  | FldType(x, None) ->
    Either.Left (make (TNVar x, make (TVar (NPName x))))
  | FldTypeVal(x, tp) ->
    Either.Left (make (TNVar x, tr_type_expr tp))
  | FldName n ->
    let pe =
      match n with
      | NLabel       -> Error.fatal (Error.desugar_error fld.pos)
      | NVar x | NOptionalVar x -> make (EVar (NPName x))
      | NImplicit n  -> make (EImplicit (NPName n))
      | NMethod   n  -> Error.fatal (Error.desugar_error fld.pos)
    in
    Either.Right (make (n, make (EPoly(pe, [], []))))
  | FldNameVal(n, e) ->
    Either.Right (make (n, tr_expr e))
| FldModule _ ->
    (* TODO: This should eventually be supported. *)
    Error.fatal (Error.desugar_error fld.pos)
  | FldEffect | FldNameAnnot _ | FldType(_, Some _) ->
    Error.fatal (Error.desugar_error fld.pos)

and tr_def ?(public=false) (def : Raw.def_data_raw) =
  let make data = { def with data = data } in
  match def.data with
  | DLet(pub, p, e) ->
    let public = public || pub in
    [ match tr_let_pattern ~public p with
      | LP_Id id -> 
        make (DLetId(id, tr_expr e))
    | LP_Fun(id, targs, iargs, args) ->
        make (DLetFun(id, targs, iargs, tr_function args (tr_expr e)))
    | LP_Pat p ->
        make (DLetPat(p, tr_expr e))
    ]
  | DMethod(pub, p, e) ->
    let public = public || pub in
    [ match tr_let_pattern ~public p with
      | LP_Id (IdVar(public, x)) ->
        make (DLetFun(IdMethod(public, x), [], [],
          make (EFn(ArgPattern (make (PId (IdVar(false, "self")))),
            tr_expr e))))
      | LP_Fun(IdVar(public, x), targs, iargs, args) ->
        let (self_arg, iargs) =
          match find_self_arg iargs with
          | None, iargs ->
            (ArgPattern (make (PId (IdVar(false, "self")))), iargs)
          | Some(_, arg), iargs -> (arg, iargs)
        in
        make (DLetFun(IdMethod(public, x), targs, iargs,
          make (EFn(self_arg, tr_function args (tr_expr e)))))
      | LP_Id (IdLabel | IdImplicit _ | IdMethod _)
      | LP_Fun((IdLabel | IdImplicit _ | IdMethod _), _, _, _)
      | LP_Pat _ ->
        Error.fatal (Error.desugar_error p.pos)
    ]
  | DMethodFn(pub, id1, id2) ->
    let public = public || pub in
    [ make (DMethodFn(public, tr_var_id (make id1), tr_var_id (make id2))) ]
  | DImplicit(n, args, sch) ->
    let args = List.map tr_named_type_arg args in
    let sch =
      match sch with
      | None -> {
          sch_pos   = def.pos;
          sch_targs = [];
          sch_named = [];
          sch_body  = make TWildcard
        }
      | Some sch -> tr_scheme_expr sch
    in
    [ make (DImplicit(n, args, sch)) ]
  | DRecord (vis, tp, flds) ->
    let (pub_type, pub_ctors) = tr_data_vis ~public def.pos vis in
    let (cd_targs, cd_named) = map_inst_like tr_scheme_field flds in
    begin match cd_targs with
    | [] -> ()
    | x :: _ ->
      Error.fatal (Error.existential_type_arg_in_record x.pos)
    end;
    begin match tr_type_def tp [] with
    | TD_Id(cd_name, args) ->
      let named_type_args = List.map tr_named_type_arg args in
      let ctor = make
        { cd_public=pub_ctors; cd_name;
          cd_targs=[]; cd_named; cd_arg_schemes=[] } in
      let dd = make (DData(pub_type, cd_name, named_type_args, [ ctor ])) in
      let method_named_args, pattern_gen =
        generate_accessor_method_pattern named_type_args cd_name in
      dd :: List.filter_map
        (create_accessor_method
          ~public:pub_ctors method_named_args pattern_gen)
        cd_named
    end
  | DData(vis, tp, cs) ->
    let (pub_type, pub_ctors) = tr_data_vis ~public def.pos vis in
    [ match tr_type_def tp [] with
      | TD_Id(x, args) ->
        make (DData(pub_type, x,
          List.map tr_named_type_arg args,
          List.map (tr_ctor_decl ~public:pub_ctors) cs))
    ]
  | DLabel(pub, pat) ->
    let public = public || pub in
    let (eff_opt, pat) = tr_pattern_with_eff_opt ~public pat in
    [ make (DLabel (eff_opt, pat)) ]
  | DHandle(pub, pat, body, hcs) ->
    let public = public || pub in
    let (eff_opt, pat) = tr_pattern_with_eff_opt ~public pat in
    let body = tr_expr body in
    let (rcs, fcs) = map_h_clauses tr_h_clause hcs in
    let body = { body with data = EHandler(body, rcs, fcs) } in
    [ make (DHandlePat(eff_opt, pat, body)) ]
  | DHandleWith(pub, pat, body) ->
    let public = public || pub in
    let (eff_opt, pat) = tr_pattern_with_eff_opt ~public pat in
    let body = tr_expr body in
    [ make (DHandlePat(eff_opt, pat, body)) ]
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

and tr_defs ?(public=false) defs = List.concat_map (tr_def ~public) @@ List.map snd defs

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

and tr_pattern_with_eff_opt ~public (pat : Raw.expr) =
  let (flds_opt, pat) = tr_pattern_with_fields ~public pat in
  (Option.map (tr_eff_opt_fields ~public) flds_opt, pat)

and tr_eff_opt_fields ~public flds =
  match flds with
  | [] -> assert false
  | [{ data = FldEffectVal tp; _ }] -> tr_type_arg ~public tp
  | { data = FldEffectVal _; _} :: fld :: _ | fld :: _ ->
    Error.fatal (Error.desugar_error fld.pos)

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
      | TA_Var(_, name, _) -> name, name ^ "#TA_Var#" ^ string_of_int i
      | TA_Effect -> "TNEffect", "TNEffect#TA_Effect#" ^ string_of_int i
      | TA_Wildcard -> "TNAnon", "TNAnon#TA_Wildcard#" ^ string_of_int i
    in
    let named_arg =
      make (TNVar old_name, make (TA_Var (false, new_name, make KWildcard))) in
    named_arg, new_name
  in
  let (new_named_type_args, new_names : named_type_arg list * ctor_name list) =
    List.split (List.mapi create_mapping named_type_args) in
  (* function for fold that generates nested series of tapps for type
     annotation *)
  let gen_tapps inner name = make (TApp(inner, make (TVar(NPName name)))) in
  let type_annot : scheme_expr =
    { sch_pos = Position.nowhere
    ; sch_targs = []
    ; sch_named = []
    ; sch_body =
      List.fold_left gen_tapps (make (TVar(NPName type_name))) new_names
    } in
  (* function that generates pattern for accessing field *)
  let pattern_gen field =
    ArgAnnot(make (PCtor(
      make (NPName type_name),
      CNParams([], [make (NVar field, make (PId(IdVar(false, field))))]),
      [])), type_annot) in
  new_named_type_args, pattern_gen

and create_accessor_method ~public named_type_args pattern_gen scheme =
  match scheme.data with
  | NVar field, _ ->
    let make data = { scheme with data } in
    (* generate accessor body, this piece of code generated accessing
    variable *)
    let e = make (EPoly(make (EVar(NPName field)), [], [])) in
    make (DLetFun(IdMethod(public, field),
      named_type_args,
      [],
      make (EFn(pattern_gen field, e))))
    |> Option.some
  | (NLabel | NImplicit _ | NMethod _ | NOptionalVar _), _ ->
    Error.warn (Error.ignored_field_in_record scheme.pos);
    None

let tr_program (p : Raw.program) =
  { p with data = tr_defs p.data }
