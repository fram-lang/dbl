(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Simple incremental SAT-solver *)

type lit = PropVar.t * bool

type 'a clause =
  { lits   : lit list;
    origin : 'a
  }

type 'a solve_result =
  | Ok
  | Error of 'a

type 'a t =
  { clauses : 'a clause list BRef.t;
    status  : 'a solve_result BRef.t;
  }

let create () =
  { clauses = BRef.create [];
    status  = BRef.create Ok
  }

(* ========================================================================= *)

let view_lits lits =
  let rec loop acc lits =
    match lits with
    | [] -> Some (PropVar.Map.bindings acc)
    | (x, pol) :: lits ->
      begin match PropVar.value x with
      | True ->
        if pol then None
        else loop acc lits
      | False ->
        if pol then loop acc lits
        else None
      | SameAs x ->
        begin match PropVar.Map.find_opt x acc with
        | None      -> loop (PropVar.Map.add x pol acc) lits
        | Some pol' ->
          if pol = pol' then loop acc lits
          else None
        end
      end
  in
  loop PropVar.Map.empty lits

(* ========================================================================= *)

exception Solver_error

let rec simplify_clauses status cls =
  let dirty = ref false in
  let simplify_clause cl =
    match view_lits cl.lits with
    | None -> None
    | Some [] ->
      BRef.set status (Error cl.origin);
      raise Solver_error
    | Some [(x, pol)] ->
      dirty := true;
      PropVar.set_bool x pol;
      None
    | Some lits -> Some { cl with lits = lits }
  in
  let cls = List.filter_map simplify_clause cls in
  if !dirty then simplify_clauses status cls
  else cls

let add_imply solver origin p1 p2 =
  let cls =
    List.map (fun lits -> { origin; lits }) (Formula.imp_to_cnf p1 p2) in
  match simplify_clauses solver.status cls with
  | cls ->
    BRef.set solver.clauses (cls @ BRef.get solver.clauses)
  | exception Solver_error ->
    ()

(* ========================================================================= *)

let solve_partial solver =
  (* TODO: do something. *)
  match BRef.get solver.status with
  | Ok  -> Ok
  | err -> err

(* ========================================================================= *)

let collect_lit_vars (pos_vars, neg_vars) (x, pol) =
  if pol then
    (PropVar.Set.add x pos_vars, neg_vars)
  else
    (pos_vars, PropVar.Set.add x neg_vars)

let collect_clause_vars vars cl =
  List.fold_left collect_lit_vars vars cl.lits

let collect_vars cls =
  List.fold_left collect_clause_vars (PropVar.Set.empty, PropVar.Set.empty)
    cls

(* ========================================================================= *)

type dom = { mutable dom_state : dom_state }
and dom_state =
  | Root of UID.t
  | Node of dom

let rec find_root dom =
  match dom.dom_state with
  | Root _ -> dom
  | Node dom' ->
    let root = find_root dom' in
    dom.dom_state <- Node root;
    root

let split_to_domains cls =
  let dom_data = Hashtbl.create 32 in
  let dom_map  = ref PropVar.Map.empty in
  let new_dom () =
    let uid = UID.fresh () in
    Hashtbl.add dom_data uid [];
    { dom_state = Root uid }
  in
  let var_dom x =
    match PropVar.Map.find_opt x !dom_map with
    | Some dom -> dom
    | None ->
      let dom = new_dom () in
      dom_map := PropVar.Map.add x dom !dom_map;
      dom
  in
  let add_clause dom cl =
    match (find_root dom).dom_state with
    | Root uid ->
      Hashtbl.replace dom_data uid (cl :: Hashtbl.find dom_data uid)
    | Node _ -> assert false
  in
  let union_dom dom1 dom2 =
    let dom1 = find_root dom1 in
    let dom2 = find_root dom2 in
    match dom1.dom_state, dom2.dom_state with
    | Root uid1, Root uid2 when uid1 = uid2 -> ()
    | Root uid1, Root uid2 ->
      let cls1 = Hashtbl.find dom_data uid1 in
      let cls2 = Hashtbl.find dom_data uid2 in
      Hashtbl.replace dom_data uid1 (cls2 @ cls1);
      Hashtbl.remove dom_data uid2;
      dom2.dom_state <- Node dom1
    | _ -> assert false
  in
  let proc_clause cl =
    match cl.lits with
    | [] -> assert false
    | (x, _) :: lits ->
      let dom = var_dom x in
      add_clause dom cl;
      List.iter (fun (y, _) -> union_dom dom (var_dom y)) lits
  in
  List.iter proc_clause cls;
  Hashtbl.to_seq_values dom_data

(* ========================================================================= *)

let rec solve_clauses status cls =
  let cls = simplify_clauses status cls in
  let (pos_vars, neg_vars) = collect_vars cls in
  let only_neg = PropVar.Set.diff neg_vars pos_vars in
  match PropVar.Set.is_empty only_neg with
  | false ->
    PropVar.Set.iter (Fun.flip PropVar.set_bool false) only_neg;
    solve_clauses status cls
  | true ->
    let only_pos = PropVar.Set.diff pos_vars neg_vars in
    begin match PropVar.Set.is_empty only_pos with
    | false ->
      PropVar.Set.iter (Fun.flip PropVar.set_bool true) only_pos;
      solve_clauses status cls
    | true ->
      Seq.iter (solve_domain status) (split_to_domains cls)
    end

and solve_domain status cls =
  match cls with
  | [] -> ()
  | { lits = []; origin } :: _ ->
    BRef.set status (Error origin);
    raise Solver_error
  | { lits = ((x, pol) :: _); _ } :: _ ->
    begin try
      BRef.bracket (fun () ->
        PropVar.set_bool x pol;
        solve_clauses status cls)
    with
    | Solver_error ->
      PropVar.set_bool x (not pol);
      solve_clauses status cls
    end

let solve_all solver =
  match BRef.get solver.status with
  | Ok ->
    begin try
      let cls = simplify_clauses solver.status (BRef.get solver.clauses) in
      Seq.iter (solve_clauses solver.status) (split_to_domains cls);
      Ok
    with
    | Solver_error ->
      begin match BRef.get solver.status with
      | Ok  -> assert false
      | err -> err
      end
    end
  | err -> err
