(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Additional environment used in type-checking definition blocks. It stores
  information about declared named implicits. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

type t = S.iname list (* in reversed order *)

type implicit = {
  i_name : string;
  i_var  : T.var;
  i_type : T.typ;
  i_used : Position.t option BRef.t
    (** The location of the first usage *)
}

type implicit_list = implicit list

let empty = []

let on_use used pos =
  match BRef.get used with
  | None   -> BRef.set used (Some pos)
  | Some _ -> ()

let generalize_implicit (env, ims) name =
  let tp   = Env.fresh_uvar env T.Kind.k_type in
  let used = BRef.ref None in
  let (env, x) =
    Env.add_mono_implicit env name tp (on_use used) in
  let im =
    { i_name = name;
      i_var  = x;
      i_type = tp;
      i_used = used
    } in
  (env, im :: ims)

let begin_generalize env ims =
  List.fold_left generalize_implicit (env, []) ims

let end_generalize_pure ims =
  ims |> List.filter_map
    (fun im ->
      match BRef.get im.i_used with
      | Some _ ->
        Some (T.NImplicit im.i_name, im.i_var, T.Scheme.of_type im.i_type)
      | None   -> None)

let end_generalize_impure ims =
  List.iter
    (fun im ->
      match BRef.get im.i_used with
      | None -> ()
      | Some pos ->
        Error.fatal (Error.ungeneralizable_implicit ~pos im.i_name))
    ims

let declare_implicit ienv name = name :: ienv

let shadow ienv name = List.filter ((<>) name) ienv

let shadow_names ienv names =
  T.Name.Map.fold
    (fun name _ ienv ->
      match name with
      | T.NLabel | T.NVar _ -> ienv
      | T.NImplicit n -> shadow ienv n)
    names ienv

let add_poly_id env ienv (id : S.ident) sch =
  match id with
  | IdVar x ->
    let (env, x) = Env.add_poly_var env x sch in
    (env, ienv, x)
  | IdImplicit n ->
    let ienv = shadow ienv n in
    let (env, x) = Env.add_poly_implicit env n sch ignore in
    (env, ienv, x)
