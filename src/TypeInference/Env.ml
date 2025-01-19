(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

open Common

(** The state of the environment. It is indexed by four types: current state
    of the environment, the two type parameters to the [opn] state of the
    module being defined, and the state that represents the rest of the
    modules. *)
type ('st, 'mod_st, 'mod_sc, 'rest_st) state =
  | StExp     : (exp, 'mod_st, 'mod_sc, 'rest_st) state
  | StModule  : ('st, closed, modl, 'st) state
  | StSection :
    ('cur_st, 'mod_st, 'mod_sc, 'rest_st) state ->
    (('cur_st, sec) opn, ('mod_st, 'mod_sc) opn, sec, 'rest_st) state

(** Stack of modules being defined. It uses a GADT to ensure that states of
    modules match the state of the environment. *)
type (_, _, _) module_stack =
  | MStack :
    ('st, 'cur_st, 'cur_sc, 'rest_st) state * 'rest_st module_stack_data ->
      ('st, 'cur_st, 'cur_sc) module_stack

and 'st module_stack_data =
  | Nil  : closed module_stack_data
  | Cons :
    ('mod_st, 'mod_sc) opn Module.t * ('st, 'mod_st, 'mod_sc) module_stack ->
      ('st, modl) opn module_stack_data

type 'st t =
  Env : {
    cur_module     : ('cur_st, 'cur_sc) opn Module.t;
      (** Currently defined module *)

    mod_stack      : ('st, 'cur_st, 'cur_sc) module_stack;
      (** Stack of modules which are currently being defined *)

    pp_tree        : PPTree.t;
      (** Pretty-printing information for types *)

    scope          : T.scope;
      (** Scope of type variables *)

    param_env      : ParamEnv.t
      (** Management of section parameters *)
  } -> 'st t

let empty =
  Env {
    cur_module     = Module.empty;
    mod_stack      = MStack (StModule, Nil);
    pp_tree        = PPTree.empty;
    scope          = T.Scope.initial;
    param_env      = ParamEnv.empty
  }

(* ========================================================================= *)

let add_existing_tvar ?pos ?(public=false) (Env env) name x =
  assert (not (T.Scope.mem env.scope x));
  Env { env with
    cur_module =
      Module.add_type_alias ~public env.cur_module name
        (T.Type.t_var x);
    pp_tree    = PPTree.add ~public ?pos env.pp_tree name (T.TVar.uid x);
    scope      = T.Scope.add env.scope x
  }

let add_existing_anon_tvar ?pos ?name (Env env) x =
  assert (not (T.Scope.mem env.scope x));
  Env { env with
    pp_tree    = PPTree.add_anon ?pos ?name env.pp_tree (T.TVar.uid x);
    scope      = T.Scope.add env.scope x
  }

let add_tvar ?pos ?public env name kind =
  let x = T.TVar.fresh kind in
  let env = add_existing_tvar ?pos ?public env name x in
  (env, x)

let add_anon_tvar ?pos ?name env kind =
  let x = T.TVar.fresh kind in
  let env = add_existing_anon_tvar ?pos ?name env x in
  (env, x)

let add_tvar_alias ?pos ?(public=false) (Env env) name x =
  Env { env with
    cur_module =
      Module.add_type_alias ~public env.cur_module name (T.Type.t_var x);
    pp_tree    = PPTree.add ~public ?pos env.pp_tree name (T.TVar.uid x)
  }

(* ========================================================================= *)

let add_existing_val ?(public=false) (Env env) name x sch =
  Env { env with
    cur_module = Module.add_val ~public env.cur_module name x sch
  }

let add_val ?public env name sch =
  let x = Var.fresh ~name:(Name.to_string name) () in
  let env = add_existing_val ?public env name x sch in
  (env, x)

let add_implicit ?public env name sch =
  add_val ?public env (NImplicit name) sch

let add_the_label env tp =
  add_implicit env the_label_name (T.Scheme.of_type tp)

let add_method ?public env owner name sch =
  add_val ?public env (NMethod(owner, name)) sch

let add_adt ?(public=false) (Env env) x info =
  Env { env with
    cur_module = Module.add_adt ~public env.cur_module x info
  }

let add_ctor ?(public=false) (Env env) name idx info =
  Env { env with
    cur_module = Module.add_ctor ~public env.cur_module name idx info
  }

(* ========================================================================= *)

let enter_section (Env env) =
  let (MStack(st, stack)) = env.mod_stack in
  Env { env with
    cur_module = Module.enter_section env.cur_module;
    mod_stack  = MStack(StSection st, stack);
    pp_tree    = PPTree.enter_section env.pp_tree
  }

let leave_section (type st) (Env env : (st, sec) opn t) : st t =
  match env.mod_stack with
  | MStack(StSection st, stack) ->
    Env { env with
      cur_module = Module.leave_section env.cur_module;
      mod_stack  = MStack(st, stack);
      pp_tree    = PPTree.leave_section env.pp_tree
    }

let declare_type ~pos (Env env) name local_name kind =
  let (MStack(StSection _, _)) = env.mod_stack in
  let (cur_module, x) = Module.declare_type env.cur_module local_name kind in
  Env { env with
    cur_module = cur_module;
    pp_tree    = PPTree.declare ~pos env.pp_tree local_name (T.TVar.uid x);
    param_env  = ParamEnv.declare_type ~pos env.param_env name x
  }

let declare_val ~pos (Env env) ~free_types ~used_types ~name ~local_name sch =
  let (MStack(StSection _, _)) = env.mod_stack in
  let (cur_module, uid) = Module.declare_val env.cur_module local_name in
  Env { env with
    cur_module = cur_module;
    param_env  =
      ParamEnv.declare_val ~pos env.param_env
        ~free_types ~used_types ~name uid sch
  }

let begin_generalize (Env env) =
  let (MStack(_, stack)) = env.mod_stack in
  let (param_env, scope, params) =
    ParamEnv.begin_generalize ~pp:env.pp_tree env.param_env env.scope in
  Env { env with
    mod_stack = MStack(StExp, stack);
    scope     = scope;
    param_env = param_env
  }, params

let check_type_param ~pos (Env env) x =
  ParamEnv.check_type_param ~pos env.param_env x

let check_val_param ~pos (Env env) uid =
  ParamEnv.check_val_param ~pos env.param_env uid

(* ========================================================================= *)

let enter_module (Env env) =
  Env { env with
    cur_module = Module.empty;
    mod_stack  = MStack(StModule, Cons(env.cur_module, env.mod_stack));
    pp_tree    = PPTree.enter_module env.pp_tree
  }

let leave_module (type st) ~public (Env env : (st, modl) opn t) name : st t =
  match env.mod_stack with
  | MStack(StModule, Cons(new_top, mod_stack)) ->
    let (pp_tree, ppm) = PPTree.leave_module ~public env.pp_tree name in
    Env { env with
      cur_module =
        Module.add_module ~public new_top name
          (Module.leave env.cur_module ppm);
      mod_stack  = mod_stack;
      pp_tree    = pp_tree
    }

let open_module ~public (Env env) m =
  Env { env with
    cur_module = Module.open_module ~public env.cur_module m;
    pp_tree    = PPTree.open_module ~public env.pp_tree (Module.pp_module m)
  }

(* ========================================================================= *)

let unit_adt =
  { Module.adt_proof =
      make_nowhere (T.EPolyFun([], [], make_nowhere T.EUnitPrf));
    Module.adt_args  = [];
    Module.adt_ctors =
      [ { ctor_name        = "()";
          ctor_targs       = [];
          ctor_named       = [];
          ctor_arg_schemes = []
        } ];
    Module.adt_type   = T.Type.t_unit;
    Module.adt_effect = Pure
  }

let option_adt =
  let a = T.TVar.fresh T.Kind.k_type in
  { Module.adt_proof = make_nowhere T.EOptionPrf;
    Module.adt_args  = [T.TNAnon, a];
    Module.adt_ctors =
      [ { ctor_name        = "None";
          ctor_targs       = [];
          ctor_named       = [];
          ctor_arg_schemes = []
        };
        { ctor_name        = "Some";
          ctor_targs       = [];
          ctor_named       = [];
          ctor_arg_schemes = [T.Scheme.of_type (T.Type.t_var a)]
        }
      ];
    Module.adt_type   = T.Type.t_option (T.Type.t_var a);
    Module.adt_effect = Pure
  }

let initial =
  let env = 
    List.fold_left
      (fun env (name, x) -> add_existing_tvar env name x)
      empty
      T.BuiltinType.all in
  let env = add_adt env T.BuiltinType.tv_unit unit_adt in
  let env = add_ctor env "()" 0 unit_adt in
  let env = add_adt env T.BuiltinType.tv_option option_adt in
  let env = add_ctor env "None" 0 option_adt in
  let env = add_ctor env "Some" 1 option_adt in
  env

(* ========================================================================= *)

type 'a lookup_fn =
  { lookup : 'st 'sc. ('st, 'sc) opn Module.t -> 'a option }

let rec lookup_stack_data : type st. _ -> st module_stack_data -> _ =
  fun lookup stack ->
  match stack with
  | Nil -> None
  | Cons(m, MStack(_, rest)) ->
    begin match lookup.lookup m with
    | Some x -> Some x
    | None   -> lookup_stack_data lookup rest
    end

let lookup_stack (Env env) lookup =
  lookup_stack_data lookup (Cons(env.cur_module, env.mod_stack))

let lookup_tvar env x =
  lookup_stack env { lookup = fun m -> Module.lookup_tvar m x }

let lookup_val env name =
  lookup_stack env { lookup = fun m -> Module.lookup_val m name }

let lookup_var env x =
  lookup_val env (NVar x)

let lookup_implicit env x =
  lookup_val env (NImplicit x)

let lookup_the_label env =
  lookup_implicit env the_label_name

let lookup_method env owner name =
  lookup_val env (NMethod(owner, name))

let lookup_ctor env name =
  lookup_stack env { lookup = fun m -> Module.lookup_ctor m name }

let lookup_adt env x =
  lookup_stack env { lookup = fun m -> Module.lookup_adt m x }

let lookup_module env name =
  lookup_stack env { lookup = fun m -> Module.lookup_module m name }

(* ========================================================================= *)
let scope (Env env) = env.scope

let level (Env env) = T.Scope.level env.scope

let pp_tree (Env env) = env.pp_tree

let fresh_uvar (Env env) kind =
  T.Type.fresh_uvar ~scope:env.scope kind
