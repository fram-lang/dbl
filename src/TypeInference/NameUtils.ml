(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on internal representation of names. *)

open Common

let method_owner_of_self (self_tp : T.typ) =
  match T.Type.whnf self_tp with
  | Whnf_Arrow   _ -> Some Name.MO_Arrow
  | Whnf_Handler _ -> Some Name.MO_Handler
  | Whnf_Label   _ -> Some Name.MO_Label
  
  | Whnf_Neutral(NH_Var owner, _) -> Some (Name.MO_TVar owner)
  | Whnf_Neutral(NH_UVar _, _)    -> None
  
  | Whnf_Effect ->
    failwith "Internal kind error"

let method_owner_of_scheme ~pos ~pp (sch : T.scheme) =
  let free_for_scheme x =
    not (List.exists (fun (_, y) -> T.TVar.equal x y) sch.sch_targs) in
  let self_sch =
    match T.Type.view sch.sch_body with
    | TArrow(sch, _, _) -> sch

    | _ ->
      Error.fatal (Error.non_arrow_method ~pos ~pp sch)
  in
  let { T.sch_targs; sch_named; sch_body = self_tp } = self_sch in
  if not (T.Scheme.is_monomorphic self_sch) then
    Error.fatal (Error.method_of_polymorphic_type ~pos ~pp sch);
  match T.Type.whnf self_tp with
  | Whnf_Arrow   _ -> Name.MO_Arrow
  | Whnf_Handler _ -> Name.MO_Handler
  | Whnf_Label   _ -> Name.MO_Label
  
  | Whnf_Neutral(NH_Var owner, _) ->
    if free_for_scheme owner then Name.MO_TVar owner
    else
      Error.fatal (Error.method_of_bound_tvar ~pos ~pp sch)
  | Whnf_Neutral(NH_UVar _, _) ->
    Error.fatal (Error.method_of_unknown_type ~pos ~pp sch)
  
  | Whnf_Effect ->
    failwith "Internal kind error"

let tr_name ~pos ~pp (name : T.name) (sch : T.scheme) =
  match name with
  | NVar x         -> Name.NVar x
  | NOptionalVar x -> Name.NOptionalVar x
  | NImplicit x    -> Name.NImplicit x
  | NMethod m      -> Name.NMethod(method_owner_of_scheme ~pos ~pp sch, m)

let tr_ident ~pos ~pp (id : S.ident) (sch : T.scheme) =
  match id with
  | IdVar x      -> Name.NVar x
  | IdImplicit x -> Name.NImplicit x
  | IdMethod m   -> Name.NMethod(method_owner_of_scheme ~pos ~pp sch, m)

let tr_scheme ~pos ~pp (sch : T.scheme) =
  { Name.sch_targs = sch.sch_targs;
    Name.sch_named =
      List.map
        (fun (name, sch) -> (tr_name ~pos ~pp name sch, sch))
        sch.sch_named;
    Name.sch_body = sch.sch_body
  }

let rename_method_owner ren (owner : Name.method_owner) =
  match owner with
  | MO_Arrow | MO_Handler | MO_Label -> owner
  | MO_TVar x  -> Name.MO_TVar (T.Ren.rename_tvar ren x)

let rename ren (name : Name.t) =
  match name with
  | NVar _ | NOptionalVar _ | NImplicit _ -> name
  | NMethod(owner, m) ->
    Name.NMethod(rename_method_owner ren owner, m)

let rename_pattern ren (name, pat, sch_expr) =
  ( rename ren name,
    T.Ren.rename_pattern ren pat,
    T.Ren.rename_scheme_expr ren sch_expr )
