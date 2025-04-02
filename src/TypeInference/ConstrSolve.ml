(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint solving. *)

open Common
open Constr

(** Try to solve list of constraints. The initial call should have [acc] empty
  and [changed] set to [false]. It returns [None] when no progress is made,
  and [Some cs'] otherwise, where [cs'] is the new list of constraints. *)
let rec try_solve_loop acc changed cs =
  match cs with
  | [] ->
    if changed then Some (List.rev acc)
    else None

  | ResolveMethod c :: cs ->
    begin match NameUtils.method_owner_of_self c.self_tp with
    | None ->
      try_solve_loop (ResolveMethod c :: acc) changed cs

    | Some _ ->
      let (e, cs') =
        ParamResolve.resolve_method
          ~vset:c.vset ~pos:c.pos c.env ~method_env:c.method_env c.mname c.sch
      in
      BRef.set c.hole (Some e);
      try_solve_loop (List.rev_append cs' acc) true cs
    end

let rec solve_partial cs =
  match try_solve_loop [] false cs with
  | None -> cs
  | Some cs -> solve_partial cs

let report_unsolved c =
  match c with
  | ResolveMethod c ->
    let pos = c.pos in
    let pp = Env.pp_tree c.env in
    Error.report (Error.cannot_resolve_method_constr ~pos ~pp c.mname c.sch)

let solve_all cs =
  let cs = solve_partial cs in
  List.iter report_unsolved cs
