(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving named parameters *)

type reinst_list = unit

let no_reinst = ()

let open_scheme ~pos env sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
let open_scheme ~pos (env : Env.t) (sch : T.scheme) =
  let (env, sch) = Env.extend_scope env sch in
  let (env, ims) =
    List.fold_left_map
      (fun env (name, sch) ->
        let (env, x) =
          match name with
          | T.NLabel -> Env.add_the_label_sch env sch
          | T.NVar x | T.NOptionalVar x -> Env.add_poly_var env x sch
          | T.NImplicit n -> Env.add_poly_implicit env n sch ignore
          | T.NMethod   n ->
            let owner = method_owner_of_scheme ~pos ~env sch in
            Env.add_poly_method env owner n sch
        in
        (env, (name, x, sch)))
      env
      sch.sch_named in
  (env, sch.sch_targs, ims, sch.sch_body)
*)

let open_scheme_explicit ~pos env sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let instantiate ~pos env rctx poly_expr sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let coerce_scheme ~pos ~name env poly_expr sch_in sch_out =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let resolve_implicit ~pos env iname sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let resolve_method ~pos env mname sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
