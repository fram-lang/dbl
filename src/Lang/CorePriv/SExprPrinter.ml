(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating Core to S-expressions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

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

let tr_var  x = Sym (Var.unique_name x)

let tr_tvar (x : _ tvar) =
  Sym (Printf.sprintf "tp%s" (UID.to_string x.uid))

let tr_tvar_ex (TVar.Ex x) = tr_tvar x

let tr_tvar_binder x =
  List [tr_tvar x; tr_kind (TVar.kind x)]

let tr_tvar_binder_ex (TVar.Ex x) =
  tr_tvar_binder x

let rec tr_type : type k. k typ -> SExpr.t =
  fun tp ->
  match tp with
  | TUnit -> Sym "unit"
  | TEffPure -> List [ Sym "effect" ]
  | TEffJoin _ -> List (Sym "effect" :: tr_effect tp)
  | TVar x -> tr_tvar x
  | TArrow  _ -> List (tr_arrow tp)
  | TForall _ -> List (Sym "forall" :: tr_forall tp)
  | TData(tp, ctors) ->
    List (Sym "data" :: tr_type tp :: List.map tr_ctor_type ctors)
  | TApp _ -> tr_type_app tp []

and tr_effect : effect -> SExpr.t list =
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

  | TUnit | TVar _ | TForall _ | TData _ | TApp _ ->
    [ Sym "->"; tr_type tp ]

and tr_forall : ttype -> SExpr.t list =
  fun tp ->
  match tp with
  | TForall(x, body) ->
    tr_tvar_binder x :: tr_forall body

  | TUnit | TVar _ | TArrow _ | TData _ | TApp _ -> [ tr_type tp ]

and tr_type_app : type k. k typ -> SExpr.t list -> SExpr.t =
  fun tp args ->
  match tp with
  | TApp(tp1, tp2) ->
    tr_type_app tp1 (tr_type tp2 :: args)

  | TUnit | TEffPure | TEffJoin _ | TVar _ | TArrow _ | TForall _ | TData _ ->
    List (Sym "app" :: tr_type tp :: args)

and tr_ctor_type { ctor_name; ctor_tvars; ctor_arg_types } =
  List (Sym ctor_name ::
    List (List.map tr_tvar_binder_ex ctor_tvars) ::
    List.map tr_type ctor_arg_types)

let tr_data_def (dd : Syntax.data_def) =
  List (
    tr_tvar_ex dd.dd_tvar ::
    tr_var  dd.dd_proof ::
    List (List.map tr_tvar_ex dd.dd_args) ::
    List.map tr_ctor_type dd.dd_ctors)

let rec tr_expr (e : Syntax.expr) =
  match e with
  | EValue v -> tr_value v
  | ELet _ | ELetPure _ | ELetIrr _ | EData _ ->
    List (Sym "defs" :: tr_defs e)
  | EApp(v1, v2) ->
    List [ Sym "app"; tr_value v1; tr_value v2 ]
  | ETApp(v, tp) ->
    List [ Sym "tapp"; tr_value v; tr_type tp ]
  | EMatch(proof, v, cls, tp, eff) ->
    List [ Sym "match"; tr_expr proof; tr_value v;
      List (Sym "clauses" :: List.map tr_clause cls);
      tr_type tp; tr_type eff ]
  | EHandle(a, x, e, h, tp, eff) ->
    List [
      Sym "handle";
      tr_tvar   a;
      tr_var    x;
      tr_expr   e;
      tr_h_expr h;
      tr_type   tp;
      tr_type   eff
    ]
  | ERepl(_, eff) ->
    List [ Sym "repl"; tr_type eff ]
  | EReplExpr(e1, tp, e2) ->
    List [ Sym "repl-expr"; tr_expr e1; Sym ("{" ^ tp ^ "}"); tr_expr e2 ]

and tr_value (v : Syntax.value) =
  match v with
  | VUnit  -> List [ Sym "unit" ]
  | VVar x -> tr_var x
  | VFn(x, tp, body) ->
    List (Sym "fn" :: List [ tr_var x; tr_type tp ] :: tr_defs body)
  | VTFun(x, body) ->
    List (Sym "tfun" :: tr_tvar_binder x :: tr_defs body)
  | VCtor(proof, n, tps, args) ->
    List (Sym "ctor" :: tr_expr proof :: Num n ::
      (List (List.map (fun (Type.Ex tp) -> tr_type tp) tps)) ::
      List.map tr_value args)

and tr_defs (e : Syntax.expr) =
  match e with
  | ELet(x, e1, e2) ->
    List [Sym "let"; tr_var x; tr_expr e1] :: tr_defs e2
  | ELetPure(x, e1, e2) ->
    List [Sym "let-pure"; tr_var x; tr_expr e1] :: tr_defs e2
  | ELetIrr(x, e1, e2) ->
    List [Sym "let-irr"; tr_var x; tr_expr e1] :: tr_defs e2
  | EData(dds, e2) ->
    List (Sym "data" :: List.map tr_data_def dds) :: tr_defs e2

  | EValue _ | EApp _ | ETApp _ | EMatch _ | EHandle _ | ERepl _
  | EReplExpr _ ->
    [ tr_expr e ]

and tr_clause (c : Syntax.match_clause) =
  List (
    List (Sym "tvars" :: List.map tr_tvar_binder_ex c.cl_tvars) ::
    List (Sym "vars" :: List.map tr_var c.cl_vars) ::
    tr_defs c.cl_body)

and tr_h_expr (h : Syntax.h_expr) =
  match h with
  | HEffect(tp_in, tp_out, x, res, body) ->
    List (
      Sym "effect" ::
      tr_type tp_in ::
      tr_type tp_out ::
      tr_var x ::
      tr_var res ::
      tr_defs body)

let tr_program = tr_expr
