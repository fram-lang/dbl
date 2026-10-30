(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The environment of the linearity checking *)

type t =
  { var_map  : Resource.typ Var.Map.t;
    rec_vars : Var.t list option
  }

let initial =
  { var_map  = Var.Map.empty;
    rec_vars = None
  }

let add_var env xs tp =
  { env with var_map = Var.Map.add xs tp env.var_map }

let add_vars env xs =
  List.fold_left (fun env x -> add_var env x TAny) env xs

let enter_rec_context env =
  assert (Option.is_none env.rec_vars);
  { env with rec_vars = Some [] }

let commit_rec_context env =
  match env.rec_vars with
  | None -> assert false
  | Some rec_vars ->
    { var_map  = 
      List.fold_left
        (fun m x -> Var.Map.add x Resource.TAny m)
        env.var_map
        rec_vars;
      rec_vars = None
    }

let add_rec_vars env xs =
  match env.rec_vars with
  | None -> assert false
  | Some rec_vars ->
    { env with rec_vars = Some (xs @ rec_vars) }

let lookup_var env xs =
  match Var.Map.find_opt xs env.var_map with
  | Some tp -> tp
  | None -> failwith "Internal error: variable not found in environment"
