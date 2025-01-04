(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Additional environment used in type-checking definition blocks. It stores
  information about declared parameters. *)

open Common

(** Parameter declaration *)
type param_decl =
  | ParamType of (** Named type parameter *)
    { pos : Position.t;
      (** Position of the declaration. *)

      name : T.tname;
      (** Name of the type parameter that will be visible in the type scheme.
        *)

      local_name : T.tname;
      (** Local name of the parameter. It is used in the type environment. *)

      tvar : T.tvar;
      (** Type variable that represents the parameter. *)
    }

  | ParamVal  of T.name * T.tvar list * T.scheme
    (** value-level parameter. It stroes the name, type variables that may
      differ for each instance, and the parameter scheme. *)

type t = param_decl list (* in reversed order *)
(*
(S.iname * T.named_tvar list * T.scheme) list (* in reversed order *)

type implicit = {
  i_name   : string;
  i_var    : T.var;
  i_scheme : T.scheme;
  i_targs  : T.named_tvar list;
  i_used   : Position.t option BRef.t
    (** The location of the first usage *)
}
*)
type param_list
(*
type implicit_list = {
  il_list : implicit list;
    (** List of implicit parameter that potentially could be generalized. *)

  il_env  : Env.t
    (** Saved environment from the place of the call of [begin_generalize] *)
}
*)
let empty = []
(*
let on_use used pos =
  match BRef.get used with
  | None   -> BRef.set used (Some pos)
  | Some _ -> ()

let rec build_implicit_type_args env sub (args : T.named_tvar list) =
  match args with
  | [] -> (env, sub, [])
  | (TNAnon, x) :: args ->
    let tp = Env.fresh_uvar env (T.TVar.kind x) in
    let sub = T.Subst.add_type sub x tp in
    build_implicit_type_args env sub args
  | (tname, x) :: args ->
    let name =
      match tname with
      | TNEffect -> "effect"
      | TNVar x  -> x
      | TNAnon -> assert false
    in
    let (env, y) = Env.add_anon_tvar env ~name (T.TVar.kind x) in
    let sub = T.Subst.rename_to_fresh sub x y in
    let (env, sub, args) = build_implicit_type_args env sub args in
    (env, sub, (tname, y) :: args)

let generalize_implicit (env, ims) (name, args, sch) =
  let used = BRef.create None in
  let (env, sub, targs) = build_implicit_type_args env T.Subst.empty args in
  let sch = T.Scheme.subst sub sch in
  let (env, x) =
    Env.add_poly_implicit env name sch (on_use used) in
  let im =
    { i_name   = name;
      i_var    = x;
      i_scheme = T.Scheme.subst sub sch;
      i_targs  = targs;
      i_used   = used
    } in
  (env, im :: ims)
*)
let begin_generalize env penv =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
let begin_generalize env ims =
  let (new_env, il_list) =
    List.fold_left generalize_implicit (Env.incr_level env, []) ims in
  (new_env, { il_list; il_env = env })
*)
let end_generalize_pure params uvs cs =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
let end_generalize_pure ims uvs =
  let level = Env.level ims.il_env in
  (* List of used implicits *)
  let ims =
    ims.il_list
    |> List.filter (fun im -> Option.is_some (BRef.get im.i_used))
  in
  (* Named abstract type parameters of used implicits *)
  let targs2 = List.concat_map (fun im -> im.i_targs) ims in
  (* Generalized implicits as named parameters *)
  let ims =
    List.map (fun im -> (T.NImplicit im.i_name, im.i_var, im.i_scheme)) ims
  in
  (* Implicitly generalized unification type variables *)
  let targs1 =
    List.fold_left
      (fun tvs (_, _, isch) -> T.Scheme.collect_uvars isch tvs)
      uvs
      ims
    |> T.UVar.Set.filter (fun x -> T.UVar.level x > level)
    |> T.UVar.Set.elements
    |> List.map (fun x -> (T.TNAnon, T.UVar.fix x))
  in
  (targs1 @ targs2, ims)
*)
let end_generalize_impure params =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let scope = Env.scope ims.il_env in
  begin match T.Type.try_shrink_scope ~scope tp with
  | Ok   () -> ()
  | Error x ->
    Error.fatal (Error.type_escapes_its_scope ~pos ~env x)
  end;
  List.iter
    (fun im ->
      match BRef.get im.i_used with
      | None -> ()
      | Some pos ->
        Error.fatal (Error.ungeneralizable_implicit ~pos im.i_name))
    ims.il_list
*)
let end_generalize_declare params penv name x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let shadow_type penv (name : T.tname) =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let declare_type ~pos penv name x kind =
  let tvar = T.TVar.fresh kind in
  let decl =
    ParamType { pos; name = tr_tname name; local_name = TNVar x; tvar } in
  decl :: shadow_type penv (TNVar x)
(*
let declare_implicit ienv name args sch = (name, args, sch) :: ienv
*)
(*
let shadow ienv name = List.filter (fun (n', _, _) -> n' <> name) ienv

let shadow_names ienv names =
  T.Name.Map.fold
    (fun name _ ienv ->
      match name with
      | T.NLabel | T.NVar _ | T.NOptionalVar _ | T.NMethod _ -> ienv
      | T.NImplicit n -> shadow ienv n)
    names ienv
*)
let add_tvar ~pos ~public env penv name kind =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_poly_id ~public env penv (id : S.ident) sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
let add_poly_id ~pos env ienv (id : S.ident) sch =
  match id with
  | IdLabel ->
    (* Labels cannot be used directly as identifiers *)
    assert false
  | IdVar(public, x) ->
    let (env, x) = Env.add_poly_var env ~public x sch in
    (env, ienv, x)
  | IdImplicit(public, n) ->
    let ienv = shadow ienv n in
    let (env, x) = Env.add_poly_implicit env ~public n sch ignore in
    (env, ienv, x)
  | IdMethod(public, name) ->
    let owner = TypeUtils.method_owner_of_scheme ~pos ~env sch in
    let (env, x) = Env.add_poly_method env ~public owner name sch in
    (env, ienv, x)
*)
let add_mono_id ~public env penv (id : S.ident) tp =
  add_poly_id ~public env penv id (T.Scheme.of_type tp)

let add_method_fn ~public env penv x name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let open_module ~public env penv m =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_partial_env env penv pat_env =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
