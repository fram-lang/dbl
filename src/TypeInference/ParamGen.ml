(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Implicit generalization of section and polymorphic parameters. This module
  provides a convenient wrappers around functions from [ParamEnv] module. *)

open Common

let end_generalize_pure ~pos params uvs cs =
  (* Collect all used parameters *)
  let (targs2, named) = ParamEnv.end_generalize_pure params in
  let targs2 = List.map (fun (name, _, x) -> (name, x)) targs2 in
  (* Collect all unification variables from generalized named parameters *)
  let uvs =
    List.fold_left
      (fun uvs (_, _, sch) ->
        T.Scheme.collect_uvars (T.SchemeExpr.to_scheme sch) uvs)
      uvs named in
  (* Generalize all unification variables *)
  let scope = ParamEnv.scope params in
  let can_be_generalized u =
    if T.UVar.in_scope u scope then false
    else begin
      T.UVar.shrink_scope u scope;
      true
    end in
  let targs1 =
    T.UVar.Set.filter can_be_generalized uvs
    |> T.UVar.Set.elements
    |> List.map (fun x -> (T.TNAnon, T.UVar.fix x)) in
  (* Fix scopes of constraints *)
  (targs1 @ targs2, named, cs)

(* ========================================================================== *)

let report_used_type (use : T.tname ParamEnv.use) =
  Error.report
    (Error.ungeneralizable_type_param
      ~pos:use.u_use_pos
      ~decl_pos:use.u_decl_pos
      use.u_name)

let report_used_val ~pp (use : Name.t ParamEnv.use) =
  Error.report
    (Error.ungeneralizable_named_param
      ~pos:use.u_use_pos
      ~decl_pos:use.u_decl_pos
      ~pp
      use.u_name)

let end_generalize_impure params uvs =
  T.UVar.Set.iter
    (fun u -> T.UVar.shrink_scope u (ParamEnv.scope params))
    uvs;
  let (used_tps, used_vals) = ParamEnv.end_generalize_impure params in
  List.iter report_used_type used_tps;
  List.iter (report_used_val ~pp:(ParamEnv.pp_tree params)) used_vals

(* ========================================================================== *)

let end_generalize_declare ~pos params env (name : S.name) id sch_expr =
  let (used_types, named) = ParamEnv.end_generalize_pure params in
  assert (List.is_empty named);
  let used_types = List.map (fun (_, uid, x) -> (uid, x)) used_types in
  let sch = T.SchemeExpr.to_scheme sch_expr in
  let free_types =
    T.Scheme.uvars sch
    |> T.UVar.Set.elements
    |> List.map (fun x -> T.UVar.fix x)
  in
  let pp = ParamEnv.pp_tree params in
  (* Check well-formedness of the scheme *)
  let (name, sch_expr) =
    begin match name with
    | NVar x -> (Name.NVar x, sch_expr)
    | NOptionalVar x ->
      begin match T.SchemeExpr.to_type_expr sch_expr with
      | Some tp ->
        (Name.NOptionalVar x, BuiltinTypes.mk_option_scheme_expr tp)
      | None ->
        Error.fatal (Error.polymorphic_optional_parameter ~pos)
      end
    | NImplicit x -> (Name.NImplicit x, sch_expr)
    | NMethod name ->
      let owner = NameUtils.method_owner_of_scheme ~pos ~pp sch in
      (Name.NMethod(owner, name), sch_expr)
    end
  in
  let local_name = NameUtils.tr_ident ~pos ~pp id sch in
  Env.declare_val ~pos env ~free_types ~used_types ~name ~local_name sch_expr
