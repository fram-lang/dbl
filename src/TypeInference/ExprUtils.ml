(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

module StrSet = Set.Make(String)

(** Make function that takes parameters of given types *)
let rec make_fun tps body_f =
  match tps with
  | [] -> body_f []
  | tp :: tps ->
    let x = Var.fresh () in
    let body = make_fun tps (fun xs -> body_f (x :: xs)) in
    { T.pos  = body.T.pos;
      T.data = T.EFn(x, tp, body)
    }

(** Make polymorphic function with given type parameters *)
let rec make_tfun tvs body =
  match tvs with
  | [] -> body
  | x :: tvs ->
    { T.pos  = body.T.pos;
      T.data = T.ETFun(x, make_tfun tvs body)
    }

(** Make function polymorphic in implicit parameters *)
let rec make_ifun ims body =
  match ims with
  | [] -> body
  | (_, x, tp) :: ims ->
    { T.pos  = body.T.pos;
      T.data = T.EFn(x, tp, make_ifun ims body)
    }

let rec make_tapp e tps =
  match tps with
  | [] -> e
  | tp :: tps ->
    let e =
      { T.pos  = e.T.pos;
        T.data = T.ETApp(e, tp)
      }
    in
    make_tapp e tps

let generalize env ims e tp =
  let tvs =
    List.fold_left
      (fun tvs (_, _, itp) -> T.Type.collect_uvars itp tvs)
      (T.Type.uvars tp)
      ims
    |> Fun.flip T.UVar.Set.diff (Env.uvars env)
    |> T.UVar.Set.elements
    |> List.map T.UVar.fix
  in
  let sch =
    { T.sch_tvars    = tvs
    ; T.sch_implicit = List.map (fun (name, _, tp) -> (name, tp)) ims
    ; T.sch_body     = tp
    }
  in
  (make_tfun tvs (make_ifun ims e), sch)

(* ========================================================================= *)

let guess_type env sub tv =
  let tp = Env.fresh_uvar env (T.TVar.kind tv) in
  (T.Subst.add_type sub tv tp, tp)

let guess_types env tvars =
  List.fold_left_map (guess_type env) T.Subst.empty tvars

(** The main instantiation function. [nset] parameter is a set of names
  currently instantiated, used to avoid infinite loops, e.g., in
  [`n : {`n : _} -> _]. *)
let rec instantiate_loop ~nset env e (sch : T.scheme) =
  let (sub, tps) = guess_types env sch.sch_tvars in
  let e = make_tapp e tps in
  let ims =
    List.map (fun (n, tp) -> (n, T.Type.subst sub tp)) sch.sch_implicit in
  let e = instantiate_implicits_loop ~nset env e ims in
  (e, T.Type.subst sub sch.sch_body)

and instantiate_implicits_loop ~nset ?(inst=[]) env e ims =
  List.fold_left (instantiate_implicit ~nset ~inst env) e ims

and instantiate_implicit ~nset ~inst env (e : T.expr) (name, tp) =
  match StrSet.mem name nset, List.assoc_opt name inst with
  | true, _ ->
    Error.fatal (Error.looping_implicit ~pos:e.pos name)
  | false, Some arg ->
    { T.pos = e.pos; T.data = T.EApp(e, arg) }
  | false, None ->
    let nset = StrSet.add name nset in
    begin match Env.lookup_implicit env name with
    | Some(x, sch, on_use) ->
      on_use e.pos;
      let arg = { T.pos = e.pos; T.data = T.EVar x } in
      let (arg, arg_tp) = instantiate_loop ~nset env arg sch in
      if Unification.subtype env arg_tp tp then
        { T.pos = e.pos; T.data = T.EApp(e, arg) }
      else
        Error.fatal
          (Error.implicit_type_mismatch ~pos:e.pos ~env name arg_tp tp)
    | None ->
      Error.fatal (Error.unbound_implicit ~pos:e.pos name)
    end

let instantiate_implicits env e ims inst =
  instantiate_implicits_loop ~nset:StrSet.empty ~inst env e ims

(* ========================================================================= *)

let ctor_func ~pos idx (info : Env.adt_info) =
  let mk_var x = { T.pos = pos; T.data = T.EVar x } in
  let ctor = List.nth info.adt_ctors idx in
  make_fun ctor.ctor_arg_types (fun xs ->
    { T.pos  = pos;
      T.data = T.ECtor(info.adt_proof, idx, List.map mk_var xs)
    })

(* ========================================================================= *)

let arg_match pat body tp eff =
  let x = Var.fresh () in
  let make data = { pat with T.data = data } in
  let body = make (T.EMatch(make (T.EVar x), [(pat, body)], tp, eff)) in
  (x, body)

let rec inst_args_match ims body tp eff =
  match ims with
  | [] -> ([], body)
  | (name, pat, x_tp) :: ims ->
    let (x, body)   = arg_match pat body tp eff in
    let (ims, body) = inst_args_match ims body tp eff in
    ((name, x, x_tp) :: ims, body)
