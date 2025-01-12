(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

open Common

let the_label_name = "~label"

type on_use = Position.t -> unit

type t = {
  cur_module : Module.t;
    (** Currently defined module *)

  mod_stack  : Module.t list;
    (** Stack of modules which are currently being defined *)

  pp_tree    : PPTree.t;
    (** Pretty-printing information for types *)

  scope      : T.scope;
    (** Scope of type variables *)
}

let empty =
  { cur_module   = Module.empty;
    mod_stack    = [];
    pp_tree      = PPTree.empty;
    scope        = T.Scope.initial;
  }

(* ========================================================================= *)

let add_existing_tvar ?pos ?(public=false) ?(on_use=ignore) env name x =
  assert (not (T.Scope.mem env.scope x));
  { env with
    cur_module =
      Module.add_type_alias ~public ~on_use env.cur_module name
        (T.Type.t_var x);
    pp_tree    = PPTree.add ~public ?pos env.pp_tree name (T.TVar.uid x);
    scope      = T.Scope.add env.scope x
  }

let add_existing_anon_tvar ?pos ?name env x =
  assert (not (T.Scope.mem env.scope x));
  { env with
    pp_tree    = PPTree.add_anon ?pos ?name env.pp_tree (T.TVar.uid x);
    scope      = T.Scope.add env.scope x
  }

let add_tvar ?pos ?public ?on_use env name kind =
  let x = T.TVar.fresh kind in
  let env = add_existing_tvar ?pos ?public ?on_use env name x in
  (env, x)

let add_anon_tvar ?pos ?name env kind =
  let x = T.TVar.fresh kind in
  let env = add_existing_anon_tvar ?pos ?name env x in
  (env, x)

let add_tvar_alias ?pos ?(public=false) env name x =
  { env with
    cur_module =
      Module.add_type_alias ~public ~on_use:ignore
        env.cur_module name (T.Type.t_var x);
    pp_tree    = PPTree.add ~public ?pos env.pp_tree name (T.TVar.uid x)
  }

(* ========================================================================= *)

let add_existing_var ?(public=false) ?(on_use=ignore) env name x sch =
  { env with
    cur_module = Module.add_var ~public ~on_use env.cur_module name x sch
  }

let add_existing_implicit ?(public=false) ?(on_use=ignore) env name x sch =
  { env with
    cur_module = Module.add_implicit ~public ~on_use env.cur_module name x sch
  }

let add_existing_method ?(public=false) ?(on_use=ignore) env owner name x sch =
  { env with
    cur_module =
      Module.add_method ~public ~on_use env.cur_module owner name x sch
  }

let add_var ?public ?on_use env name sch =
  let x = Var.fresh ~name () in
  let env = add_existing_var ?public ?on_use env name x sch in
  (env, x)

let add_implicit ?(public=false) ?(on_use=ignore) env name sch =
  let x = Var.fresh ~name () in
  let env = add_existing_implicit ~public ~on_use env name x sch in
  (env, x)

let add_the_label env tp =
  add_implicit env the_label_name (T.Scheme.of_type tp)

let add_method ?(public=false) ?(on_use=ignore) env owner name sch =
  let x = Var.fresh ~name () in
  let env = add_existing_method ~public ~on_use env owner name x sch in
  (env, x)

let add_method_fn ~public env x name =
  { env with
    cur_module =
      Module.add_method_fn ~public ~on_use:ignore env.cur_module x name
  }

let add_adt ?(public=false) env x info =
  { env with
    cur_module = Module.add_adt ~public env.cur_module x info
  }

let add_ctor ?(public=false) env name idx info =
  { env with
    cur_module = Module.add_ctor ~public env.cur_module name idx info
  }

(* ========================================================================= *)

let enter_module env =
  { env with
    cur_module = Module.empty;
    mod_stack  = env.cur_module :: env.mod_stack;
    pp_tree    = PPTree.enter_module env.pp_tree
  }

let leave_module ~public env name =
  match env.mod_stack with
  | [] -> assert false
  | new_top :: mod_stack ->
    let (pp_tree, ppm) = PPTree.leave_module ~public env.pp_tree name in
    { env with
      cur_module =
        Module.add_module ~public new_top name
          (Module.leave env.cur_module ppm);
      mod_stack  = mod_stack;
      pp_tree    = pp_tree
    }

let open_module ~public env m =
  { env with
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

let lookup_stack env lookup =
  List.find_map lookup (env.cur_module :: env.mod_stack)

let lookup_tvar env x =
  lookup_stack env (fun m -> Module.lookup_tvar m x)

let lookup_var env x =
  lookup_stack env (fun m -> Module.lookup_var m x)
  
let lookup_implicit env x =
  lookup_stack env (fun m -> Module.lookup_implicit m x)

let lookup_the_label env =
  lookup_implicit env the_label_name

let lookup_method env owner name =
  lookup_stack env (fun m -> Module.lookup_method m owner name)

let lookup_ctor env name =
  lookup_stack env (fun m -> Module.lookup_ctor m name)

let lookup_adt env x =
  lookup_stack env (fun m -> Module.lookup_adt m x)

let lookup_module env name =
  lookup_stack env (fun m -> Module.lookup_module m name)

(* ========================================================================= *)

let incr_level env =
  { env with scope = T.Scope.incr_level env.scope }

let scope env = env.scope

let level env = T.Scope.level env.scope

let pp_tree env = env.pp_tree

let fresh_uvar env kind =
  T.Type.fresh_uvar ~scope:env.scope kind
