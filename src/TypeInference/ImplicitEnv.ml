(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Additional environment used in type-checking definition blocks. It stores
  information about declared named implicits. *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

type t = S.name list (* in reversed order *)

type implicit = {
  i_name : T.name;
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
  List.map (fun im -> (im.i_name, im.i_var, im.i_type)) ims

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
