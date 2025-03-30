(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common functions of pretty printers *)

type ctx = 
  { mutable kuvar_list : (Kind.kuvar * string) list;
      (** Association list with the names of kind unification variables. *)

    mutable uvar_map : string UID.Map.t;
      (** Names associated to unification variables. This is a map from UIDs,
        because unification variables have different representation in
        languages that use this module. *)

            used_uvars : (string, unit) Hashtbl.t;
      (** Names already used to pretty-print unification variables *)

    mutable tvar_map : string TVar.Map.t;
      (** Names associated to anonymous or non-printable type variables *)

            used_tvars : (string, unit) Hashtbl.t;
      (** Names already used to pretty-print type variables *)

    mutable anon_tvar_pos : (Position.t * string) list
      (** Position of definitions of anonymous or non-printable type
        variables. *)
  }

let empty_context () =
  { kuvar_list    = [];
    uvar_map      = UID.Map.empty;
    used_uvars    = Hashtbl.create 42;
    tvar_map      = TVar.Map.empty;
    used_tvars    = Hashtbl.create 42;
    anon_tvar_pos = []
  }

(* ========================================================================= *)

(** Internal environment of the pretty printer *)
module Env : sig
  type t
  
  (** Create a new environment *)
  val create : ctx -> PPTree.t -> t

  (** Register name for a type variable *)
  val add_tvar : t -> string -> TVar.t -> t

  (** Register given anonymous variable name as used. *)
  val add_anon_tvar : t -> Position.t option -> string -> TVar.t -> unit

  (** Get the context *)
  val get_context : t -> ctx

  (** Lookup a type variable representation *)
  val lookup_tvar : t -> TVar.t -> PPTree.pp_result

  (** Add a string to the output *)
  val print_string : t -> string -> unit

  (** Get the contents of the buffer with the output *)
  val buf_contents : t -> string

  (** Check if given name for anonymous type variable was not used before. *)
  val is_anon_tvar_fresh : t -> string -> bool
end = struct
  type t =
    { local_names : string TVar.Map.t;
      pp_tree     : PPTree.t;
      context     : ctx;
      buffer      : Buffer.t
    }

  let create ctx pp_tree =
    { local_names = TVar.Map.empty;
      pp_tree     = pp_tree;
      context     = ctx;
      buffer      = Buffer.create 80
    }

  let add_tvar env name x =
    { env with local_names = TVar.Map.add x name env.local_names }

  let add_anon_tvar env pos name x =
    env.context.tvar_map <- TVar.Map.add x name env.context.tvar_map;
    Hashtbl.add env.context.used_tvars name ();
    match pos with
    | None -> ()
    | Some pos ->
      env.context.anon_tvar_pos <- (pos, name) :: env.context.anon_tvar_pos

  let get_context env = env.context

  let lookup_tvar env x =
    match TVar.Map.find_opt x env.local_names with
    | Some name -> PPTree.Found name
    | None ->
      begin match PPTree.lookup env.pp_tree (TVar.pp_uid x) with
      | Found name -> PPTree.Found name
      | _ as result ->
        begin match TVar.Map.find_opt x env.context.tvar_map with
        | Some name -> PPTree.Found name
        | None -> result
        end
      end

  let print_string env str =
    Buffer.add_string env.buffer str

  let buf_contents env =
    Buffer.contents env.buffer

  let is_anon_tvar_fresh env name =
    not (Hashtbl.mem env.context.used_tvars name)
end

let paren env prec eprec pp =
  if prec > eprec then Env.print_string env "(";
  pp ();
  if prec > eprec then Env.print_string env ")"

(* ========================================================================= *)

let kuvar_to_string env u =
  let ctx = Env.get_context env in
  let kuvar_name (u', name) =
    if Kind.KUVar.equal u u' then Some name
    else None
  in
  match List.find_map kuvar_name ctx.kuvar_list with
  | Some name -> name
  | None ->
    let n = List.length ctx.kuvar_list in
    let name = "?k" ^ string_of_int n in
    ctx.kuvar_list <- (u, name) :: ctx.kuvar_list;
    name

let gen_tvar_name is_fresh base =
  let rec loop n =
    let name = Printf.sprintf "%s%d" base n in
    if is_fresh name then name
    else loop (n+1)
  in
  if is_fresh base then base
  else loop 0

let tvar_to_string env x =
  match Env.lookup_tvar env x with
  | Found name -> name
  | Anon(base, pos) ->
    let name = gen_tvar_name (Env.is_anon_tvar_fresh env) ("#" ^ base) in
    Env.add_anon_tvar env pos name x;
    name
  | Unbound _ ->
    let name = gen_tvar_name (Env.is_anon_tvar_fresh env) "#?T" in
    Env.add_anon_tvar env None name x;
    name

let gen_uvar_name is_fresh =
  let rec gen2 n =
    let name = Printf.sprintf "a%d" n in
    if is_fresh name then name
    else gen2 (n+1)
  in
  let rec gen1 c =
    let name = Printf.sprintf "%c" c in
    if is_fresh name then name
    else if c >= 'z' then gen2 0
    else gen1 (Char.chr (Char.code c + 1))
  in
  gen1 'a'

let is_uvar_name_fresh ctx env name =
  not (Hashtbl.mem ctx.used_uvars name)

let uvar_to_string env u =
  let ctx = Env.get_context env in
  match UID.Map.find_opt u ctx.uvar_map with
  | Some name -> name
  | None ->
    let name = gen_uvar_name (is_uvar_name_fresh ctx env) in
    ctx.uvar_map <- UID.Map.add u name ctx.uvar_map;
    Hashtbl.add ctx.used_uvars name ();
    name

(* ========================================================================= *)

type effect_tree =
  | PP_EffWildcard
  | PP_EffVar         of TVar.t
  | PP_EffUVar        of UID.t
  | PP_EffSimpleGuard of effect_tree

type type_tree =
  | PP_TVar       of TVar.t
  | PP_TUVar      of UID.t
  | PP_TPureArrow of scheme_tree * type_tree
  | PP_TArrow     of scheme_tree * type_tree * effect_tree list
  | PP_TLabel     of effect_tree list * type_tree * effect_tree list
  | PP_THandler   of
    { eff_var : TVar.t;
      cap_tp  : type_tree;
      in_tp   : type_tree;
      in_eff  : effect_tree list;
      out_tp  : type_tree;
      out_eff : effect_tree list
    }
  | PP_TEffect    of effect_tree list
  | PP_TApp       of type_tree * type_tree

and scheme_tree =
  { ppsch_targs : (Names.tname * TVar.t) list;
    ppsch_named : (Names.name * scheme_tree) list;
    ppsch_body  : type_tree
  }

(* ========================================================================= *)

let fresh_for_tvar env name x =
  match Env.lookup_tvar env x with
  | Found name' -> name <> name'
  | _ -> true

let rec fresh_for_effect env name (eff : effect_tree) =
  match eff with
  | PP_EffWildcard | PP_EffUVar _ -> true
  | PP_EffVar x -> fresh_for_tvar env name x
  | PP_EffSimpleGuard eff -> fresh_for_effect env name eff

let fresh_for_effects env name effs =
  List.for_all (fresh_for_effect env name) effs

let rec fresh_for_type env name (tp : type_tree) =
  match tp with
  | PP_TVar  x -> fresh_for_tvar env name x
  | PP_TUVar _ -> true
  | PP_TPureArrow(sch, tp) ->
    fresh_for_scheme env name sch && fresh_for_type env name tp
  | PP_TArrow(sch, tp, eff) ->
    fresh_for_scheme  env name sch &&
    fresh_for_type    env name tp &&
    fresh_for_effects env name eff
  | PP_TLabel(eff, delim_tp, delim_eff) ->
    fresh_for_effects env name eff &&
    fresh_for_type    env name delim_tp &&
    fresh_for_effects env name delim_eff
  | PP_THandler h ->
    fresh_for_type    env name h.cap_tp &&
    fresh_for_type    env name h.in_tp &&
    fresh_for_effects env name h.in_eff &&
    fresh_for_type    env name h.out_tp &&
    fresh_for_effects env name h.out_eff
  | PP_TEffect eff -> fresh_for_effects env name eff
  | PP_TApp(tp1, tp2) ->
    fresh_for_type env name tp1 && fresh_for_type env name tp2

and fresh_for_scheme env name (sch : scheme_tree) =
  let { ppsch_targs = _; ppsch_named = named; ppsch_body = body } = sch in
  List.for_all (fun (_, sch) -> fresh_for_scheme env name sch) named &&
  fresh_for_type env name body

(* ========================================================================= *)

let rec print_kind env k prec =
  match Kind.view k with
  | KType   -> Env.print_string env "type"
  | KEffect -> Env.print_string env "effect"
  | KUVar u -> Env.print_string env (kuvar_to_string env u)
  | KArrow(k1, k2) ->
    paren env prec 0 (fun () ->
      print_kind env k1 1;
      Env.print_string env " -> ";
      print_kind env k2 0)

let print_tvar env x prec =
  Env.print_string env (tvar_to_string env x)

let print_uvar env u prec =
  Env.print_string env (uvar_to_string env u)

let rec print_single_effect env (eff : effect_tree) =
  match eff with
  | PP_EffWildcard -> Env.print_string env "_"
  | PP_EffVar  x   -> print_tvar env x 0
  | PP_EffUVar u   -> print_uvar env u 0
  | PP_EffSimpleGuard eff ->
    print_single_effect env eff;
    Env.print_string env "?"

let print_effect_body env effs =
  match effs with
  | [] -> ()
  | eff :: effs ->
    print_single_effect env eff;
    effs |> List.iter (fun eff ->
      Env.print_string env ",";
      print_single_effect env eff)

let print_effect env effs prec =
  match effs with
  | [eff] -> print_single_effect env eff
  | _ ->
    Env.print_string env "[";
    print_effect_body env effs;
    Env.print_string env "]"

let rec print_type env (tp : type_tree) prec =
  match tp with
  | PP_TVar  x -> print_tvar env x prec
  | PP_TUVar u -> print_uvar env u prec

  | PP_TPureArrow(sch, tp) ->
    paren env prec 0 (fun () ->
      print_scheme env sch 1;
      Env.print_string env " -> ";
      print_type env tp 0)

  | PP_TArrow(sch, tp, [ PP_EffWildcard ]) ->
    paren env prec 0 (fun () ->
      print_scheme env sch 1;
      Env.print_string env " ->> ";
      print_type env tp 0)

  | PP_TArrow(sch, tp, effs) ->
    paren env prec 0 (fun () ->
      print_scheme env sch 1;
      Env.print_string env " ->[";
      print_effect_body env effs;
      Env.print_string env "] ";
      print_type env tp 0)

  | PP_TLabel(eff, delim_tp, delim_eff) ->
    paren env prec 0 (fun () ->
      Env.print_string env "label ";
      print_effect env eff 11;
      Env.print_string env " [";
      print_effect_body env delim_eff;
      Env.print_string env "] ";
      print_type env delim_tp 11)

  | PP_THandler h ->
    paren env prec 0 (fun () ->
      let name = gen_tvar_name (fun n -> fresh_for_type env n tp) "E" in
      let env1 = Env.add_tvar env name h.eff_var in
      Env.print_string env "handler ";
      Env.print_string env name;
      Env.print_string env " of ";
      print_type env1 h.cap_tp 1;
      Env.print_string env " with [";
      print_effect_body env1 h.in_eff;
      Env.print_string env "] ";
      print_type env1 h.in_tp 1;
      begin match h.out_eff with
      | [ PP_EffWildcard ] ->
        Env.print_string env " ->> "
      | effs ->
        Env.print_string env " ->[";
        print_effect_body env effs;
        Env.print_string env "] "
      end;
      print_type env h.out_tp 1)

  | PP_TEffect eff -> print_effect env eff prec

  | PP_TApp(tp1, tp2) ->
    paren env prec 10 (fun () ->
      print_type env tp1 10;
      Env.print_string env " ";
      print_type env tp2 11)

and print_scheme env (sch : scheme_tree) prec =
  match sch.ppsch_targs, sch.ppsch_named with
  | [], [] -> print_type env sch.ppsch_body prec
  | _ ->
    paren env prec 0 (fun () ->
      let env =
        print_scheme_args sch env "{" sch.ppsch_targs sch.ppsch_named in
      Env.print_string env "} -> ";
      print_type env sch.ppsch_body 0)

and print_scheme_args sch env sep targs nargs =
  match targs with
  | [] ->
    print_scheme_named_args env sep nargs;
    env
  | (tname, x) :: targs ->
    Env.print_string env sep;
    let xname =
      match tname with
      | TNAnon ->
        let xname = gen_tvar_name (fun n -> fresh_for_scheme env n sch) "T" in
        Env.print_string env "type ";
        Env.print_string env xname;
        xname
      | TNVar xn ->
        let xname = gen_tvar_name (fun n -> fresh_for_scheme env n sch) xn in
        Env.print_string env xn;
        if xname <> xn then begin
          Env.print_string env "=";
          Env.print_string env xname
        end;
        xname
    in
    let env = Env.add_tvar env xname x in
    print_scheme_args sch env ", " targs nargs

and print_scheme_named_args env sep nargs =
  match nargs with
  | [] -> ()
  | (name, sch) :: nargs ->
    Env.print_string env sep;
    let sch = 
      begin match name with
      | NVar x         -> Env.print_string env x; sch
      | NOptionalVar x -> 
        Env.print_string env "?";
        Env.print_string env x;
        begin match sch.ppsch_body with
        | PP_TApp(_, tp) ->
          { ppsch_targs = []; ppsch_named = []; ppsch_body = tp };
        (* Error here? *)
        | _           -> sch 
        end
      | NImplicit n -> Env.print_string env n; sch
      | NMethod n ->
        Env.print_string env "method ";
        Env.print_string env n; sch
      end
    in
    Env.print_string env ":";
    print_scheme env sch 0;
    print_scheme_named_args env ", " nargs

(* ========================================================================= *)

let run_printer ctx pp_tree print x =
  let env = Env.create ctx pp_tree in
  print env x 0;
  Env.buf_contents env

let pp_kind ctx k =
  run_printer ctx PPTree.empty print_kind k

let pp_tvar ctx pp_tree x =
  run_printer ctx pp_tree print_tvar x

let pp_effect_trees ctx pp_tree eff =
  run_printer ctx pp_tree print_effect eff

let pp_type_tree ctx pp_tree tp =
  run_printer ctx pp_tree print_type tp

let pp_scheme_tree ctx pp_tree sch =
  run_printer ctx pp_tree print_scheme sch

(* ========================================================================= *)

let additional_info ctx =
  let buf = Buffer.create 80 in
  List.rev ctx.anon_tvar_pos |> List.iter (fun (pos, name) ->
    Printf.bprintf buf "\n  Type %s is defined at %s"
      name
      (Position.to_string pos));
  Buffer.contents buf
