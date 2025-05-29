(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the effect inference *)

open Common

type var_info =
  | Simple of T.scheme
  | Large  of T.tvar list * T.constr list * T.scheme

type adt_info =
  { adt_proof    : T.var;
    adt_args     : T.named_tvar list;
    adt_type     : T.typ; (* Already applied to [adt_args] *)
    adt_ctors    : T.ctor_decl list;
    adt_positive : bool
  }

type t =
  { tvar_map : T.tvar S.TVar.Map.t;
    (** Map from type variables to their ConE representation *)

    var_map : var_info Var.Map.t;
    (** Map from variables to their type schemes *)

    adt_map : adt_info Var.Map.t;
    (** Map from ADT shape variables to their information *)

    scope : Scope.t;
    (** Current scope of type variables *)

    constr : ConstrSet.t;
    (** Set of collected constraints *)

    rec_vars : (T.var * T.scheme) list option;
    (** List of variables defined in a recursive block, or [None] if the
      environment is not in a recursive block or recursive block was
      committed *)

    sat_solver : (T.tvar * origin) IncrSAT.Solver.t;
    (** global SAT-solver, common to all environments. *)

    solve_all : bool
    (** A flag determining the behavior of constraint solving. See
      [EffectInference.Main] for details. *)
  }

let initial ~solve_all () =
  let add_builtin tvar_map (name, x) =
    let y = List.assoc name T.BuiltinType.all in
    S.TVar.Map.add x y tvar_map
  in
  let tvar_map =
    List.fold_left add_builtin S.TVar.Map.empty S.BuiltinType.all in
  { tvar_map   = tvar_map;
    var_map    = Var.Map.empty;
    adt_map    = Var.Map.empty;
    scope      = Scope.initial;
    constr     = ConstrSet.create ();
    rec_vars   = None;
    sat_solver = IncrSAT.Solver.create ();
    solve_all  = solve_all
  }

let add_tvar env x =
  let y = T.TVar.clone_unif ~scope:env.scope x in
  { env with
    tvar_map = S.TVar.Map.add x y env.tvar_map
  }, y

let add_named_tvar env (name, x) =
  let (env, x) = add_tvar env x in
  (env, (name, x))

let add_named_tvars env targs =
  List.fold_left_map add_named_tvar env targs

let add_data env x adt_info =
  assert (not (Var.Map.mem x env.var_map));
  { env with
    adt_map = Var.Map.add x adt_info env.adt_map
  }

let add_poly_var env x sch =
  assert (not (Var.Map.mem x env.var_map));
  { env with
    var_map = Var.Map.add x (Simple sch) env.var_map
  }

let add_mono_var env x tp =
  add_poly_var env x (T.Scheme.of_type tp)

let add_lpoly_var env x evs cs sch =
  assert (not (Var.Map.mem x env.var_map));
  { env with
    var_map = Var.Map.add x (Large(evs, cs, sch)) env.var_map
  }

let add_rec_var env x sch =
  match env.rec_vars with
  | None -> assert false
  | Some rec_vars ->
    { env with
      rec_vars = Some ((x, sch) :: rec_vars)
    }

let lookup_tvar env x =
  match S.TVar.Map.find_opt x env.tvar_map with
  | Some y -> y
  | None ->
    InterpLib.InternalError.report ~reason:"Unbound type variable" ()

let lookup_var env x =
  match Var.Map.find_opt x env.var_map with
  | Some info -> info
  | None ->
    InterpLib.InternalError.report ~reason:"Unbound variable" ()

let lookup_adt env x =
  match Var.Map.find_opt x env.adt_map with
  | Some info -> info
  | None ->
    InterpLib.InternalError.report ~reason:"Unbound ADT proof variable" ()

(* ========================================================================= *)

let enter_scope env =
  { env with
    scope  = Scope.enter env.scope;
    constr = ConstrSet.create ()
  }

let enter_rec_context env =
  assert (Option.is_none env.rec_vars);
  { env with
    rec_vars = Some []
  }

let commit_rec_context env =
  match env.rec_vars with
  | None -> assert false
  | Some rec_vars ->
    let env = { env with rec_vars = None } in
    List.fold_left (fun env (x, sch) -> add_poly_var env x sch) env rec_vars

let add_constraints env cs =
  ConstrSet.add_list env.constr cs

let constraints env =
  ConstrSet.to_list env.constr

let clear_constraints env =
  ConstrSet.clear env.constr

let add_formula_constraint env ~origin x p1 p2 =
  IncrSAT.Solver.add_imply env.sat_solver (x, origin) p1 p2

let sat_solver env = env.sat_solver

let solve_all_is_set env = env.solve_all

let scope env = env.scope

let all_tvars env =
  S.TVar.Map.bindings env.tvar_map |> List.map snd

let fresh_gvar env =
  T.Effct.fresh_gvar ~scope:env.scope
