(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating Core to S-expressions *)

open TypeBase
open SExpr

let rec tr_kind : type k. k kind -> SExpr.t =
  fun k ->
  match k with
  | KType   -> Sym "type"
  | KEffect -> Sym "effect"
  | KArrow _ -> List (tr_arrow_kind k)

and tr_arrow_kind : type k. k kind -> SExpr.t list =
  fun k ->
  match k with
  | KArrow(k1, k2) ->
    tr_kind k1 :: tr_arrow_kind k2
  | KType | KEffect -> [ Sym "->"; tr_kind k ]

let tr_rflag rflag =
  match rflag with
  | Positive -> Sym "+"
  | General  -> Sym "o"

let tr_var  x = Sym (Var.unique_name x)

let tr_tvar (x : _ tvar) =
  Sym (Printf.sprintf "tp%s" (UID.to_string x.uid))

let tr_uvar (x : UID.t) =
  Sym (Printf.sprintf "uvar%s" (UID.to_string x))

let tr_tvar_ex (TVar.Ex x) = tr_tvar x

let tr_tvar_binder x =
  List [tr_tvar x; tr_kind (TVar.kind x)]

let tr_tvar_binder_ex (TVar.Ex x) =
  tr_tvar_binder x

let rec tr_type : type k. k typ -> SExpr.t =
  fun tp ->
  match tp with
  | TEffPure -> List [ Sym "effect" ]
  | TEffJoin _ -> List (Sym "effect" :: tr_effect tp)
  | TVar x -> tr_tvar x
  | TArrow  _ -> List (tr_arrow tp)
  | TForall _ -> List (Sym "forall" :: tr_forall tp)
  | TGuard(cs, tp) ->
    List [Sym "guard"; List (List.map tr_constr cs); tr_type tp ]
  | TLabel lbl ->
    List
      [ Sym "label";
        tr_type lbl.effct;
        List (List.map tr_tvar_binder_ex lbl.tvars);
        List (List.map tr_type lbl.val_types);
        tr_type lbl.delim_tp;
        tr_type lbl.delim_eff;
        tr_rflag lbl.rflag ]
  | TData(tp, rflag, ctors) ->
    List (Sym "data" :: tr_type tp :: tr_rflag rflag ::
      List.map tr_ctor_type ctors)
  | TApp _ -> tr_type_app tp []

and tr_effect : effct -> SExpr.t list =
  fun tp ->
  match tp with
  | TEffPure -> []
  | TEffJoin(eff1, eff2) ->
    tr_effect eff1 @ tr_effect eff2
  | TVar x -> [ tr_tvar x ]
  | TApp _ -> [ tr_type tp ]

and tr_arrow : ttype -> SExpr.t list =
  fun tp ->
  match tp with
  | TArrow(tp1, tp2, eff) when Effect.is_pure eff ->
    tr_type tp1 :: tr_arrow tp2
  | TArrow(tp1, tp2, eff) ->
    [ tr_type tp1; Sym "->"; tr_type tp2; tr_type eff ]

  | TVar _ | TForall _ | TGuard _ | TLabel _ | TData _ | TApp _ ->
    [ Sym "->"; tr_type tp ]

and tr_forall : ttype -> SExpr.t list =
  fun tp ->
  match tp with
  | TForall(x, body) ->
    tr_tvar_binder x :: tr_forall body

  | TVar _ | TArrow _ | TGuard _ | TLabel _ | TData _ | TApp _ ->
    [ tr_type tp ]

and tr_type_app : type k. k typ -> SExpr.t list -> SExpr.t =
  fun tp args ->
  match tp with
  | TApp(tp1, tp2) ->
    tr_type_app tp1 (tr_type tp2 :: args)

  | TEffPure | TEffJoin _ | TVar _ | TArrow _ | TForall _ | TGuard _
  | TLabel _ | TData _ ->
    List (Sym "app" :: tr_type tp :: args)

and tr_ctor_type { ctor_name; ctor_tvars; ctor_arg_types } =
  List (Sym ctor_name ::
    List (List.map tr_tvar_binder_ex ctor_tvars) ::
    List.map tr_type ctor_arg_types)

and tr_constr (eff1, eff2) =
  List [ tr_type eff1; Sym "<:"; tr_type eff2 ]

let tr_type_ex (Type.Ex tp) =
  tr_type tp

let tr_data_def (dd : Syntax.data_def) =
  match dd with
  | DD_Data adt ->
    List (
      Sym "data" ::
      tr_tvar_ex adt.tvar ::
      tr_var  adt.proof ::
      List (List.map tr_tvar_ex adt.args) ::
      List.map tr_ctor_type adt.ctors)

  | DD_Label lbl ->
    List [
      Sym "label";
      tr_tvar lbl.tvar;
      tr_var lbl.var;
      tr_type lbl.delim_tp;
      tr_type lbl.delim_eff
    ]

let tr_lit (l : Syntax.lit) =
  match l with
  | LNum   n -> string_of_int n
  | LNum64 n -> Int64.to_string n ^ "L"
  | LStr   s -> Printf.sprintf "\"%s\"" (String.escaped s)

let rec tr_expr (e : Syntax.expr) =
  match e with
  | EValue v -> tr_value v
  | ELet _ | ELetPure _ | ELetRec _ | ERecCtx _ | EData _ | EReset _ ->
    List (Sym "defs" :: tr_defs e)
  | EFn _ | ETFun _ | ECAbs _ ->
    List (Sym "fn" :: tr_funs e)
  | EApp _ | ETApp _ | ECApp _ ->
    List (Sym "app" :: tr_apps e [])
  | ECtor(proof, n, tps, args) ->
    List (Sym "ctor" :: tr_expr proof :: Num n ::
      (List (List.map (fun (Type.Ex tp) -> tr_type tp) tps)) ::
      List.map tr_value args)
  | EMatch(proof, v, cls, tp, eff) ->
    List [ Sym "match"; tr_expr proof; tr_value v;
      List (Sym "clauses" :: List.map tr_clause cls);
      tr_type tp; tr_type eff ]
  | EShift(v, tvs, xs, k, body, tp) ->
    List
      [ Sym "shift";
        tr_value v;
        List (List.map tr_tvar_binder_ex tvs);
        List (List.map tr_var xs);
        tr_var k;
        tr_type tp;
        tr_expr body ]
  | ERepl(_, tp, eff) ->
    List [ Sym "repl"; tr_type tp; tr_type eff ]
  | EReplExpr(e1, tp, e2) ->
    List [ Sym "repl-expr"; tr_expr e1; Sym ("{" ^ tp ^ "}"); tr_expr e2 ]

and tr_value (v : Syntax.value) =
  match v with
  | VLit l -> List [ Sym (tr_lit l) ]
  | VVar x -> tr_var x
  | VExtern(name, tp) ->
    List [ Sym "extern"; Sym name; tr_type tp ]

and tr_defs (e : Syntax.expr) =
  match e with
  | ELet(x, e1, e2) ->
    List [Sym "let"; tr_var x; tr_expr e1] :: tr_defs e2
  | ELetPure(Relevant, x, e1, e2) ->
    List [Sym "let-pure"; tr_var x; tr_expr e1] :: tr_defs e2
  | ELetPure(Irrelevant, x, e1, e2) ->
    List [Sym "let-irr"; tr_var x; tr_expr e1] :: tr_defs e2
  | ELetRec(rds, e2) ->
    List (Sym "let-rec" :: List.map tr_rec_def rds) :: tr_defs e2
  | ERecCtx e ->
    List [Sym "rec-ctx"] :: tr_defs e
  | EData(dds, e2) ->
    List (Sym "data" :: List.map tr_data_def dds) :: tr_defs e2
  | EReset(v, tps, vs, body, x, ret) ->
    List
      [ Sym "reset";
        tr_value v;
        List (List.map tr_type_ex tps);
        List (List.map tr_value vs);
        tr_var x; tr_expr ret
      ] :: tr_defs body

  | EValue _ | EFn _ | ETFun _ | ECAbs _ | EApp _ | ETApp _ | ECApp _
  | ECtor _ | EMatch _ | EShift _ | ERepl _ | EReplExpr _ ->
    [ tr_expr e ]

and tr_rec_def (x, tp, body) =
  List [ tr_var x; tr_type tp; tr_expr body ]

and tr_funs (e : Syntax.expr) =
  match e with
  | EFn(x, tp, body) ->
    List [ tr_var x; tr_type tp ] :: tr_funs body
  | ETFun(x, body) ->
    List [Sym "type"; tr_tvar_binder x] :: tr_defs body
  | ECAbs(cs, body) ->
    List (Sym "constr" :: List.map tr_constr cs) :: tr_funs body

  | EValue _ | ELet _ | ELetPure _ | ELetRec _ | ERecCtx _ | EApp _ | ETApp _
  | ECApp _ | EData _ | ECtor _ | EMatch _ | EShift _ | EReset _ | ERepl _
  | EReplExpr _ ->
    [tr_expr e]

and tr_apps (e : Syntax.expr) args =
  match e with
  | EApp(e1, v2) -> tr_apps e1 (tr_value v2 :: args)
  | ETApp(e1, tp) -> tr_apps e1 (List [Sym "type"; tr_type tp] :: args)
  | ECApp e1 -> tr_apps e1 (Sym "constr" :: args)

  | EValue _ | ELet _ | ELetPure _ | ELetRec _ | ERecCtx _ | EFn _ | ETFun _
  | ECAbs _ | EData _ | ECtor _ | EMatch _ | EShift _ | EReset _ | ERepl _
  | EReplExpr _ ->
    tr_expr e :: args

and tr_clause (c : Syntax.match_clause) =
  List (
    List (Sym "tvars" :: List.map tr_tvar_binder_ex c.cl_tvars) ::
    List (Sym "vars" :: List.map tr_var c.cl_vars) ::
    tr_defs c.cl_body)

let tr_program = tr_expr
