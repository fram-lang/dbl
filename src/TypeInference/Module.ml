(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Represention of module definitions *)

open Common

module StrMap = Map.Make(String)

type on_use = Position.t -> unit

(*
type 'data ident_info = { data : 'data; public : bool }
*)
(** Information about an ADT definition *)
type adt_info = {
  adt_proof  : T.poly_expr;
  adt_args   : T.named_tvar list;
  adt_ctors  : T.ctor_decl list;
  adt_type   : T.typ;
  adt_effect : T.effect
}

type var_info =
  | VI_Var      of T.var * T.scheme
  | VI_Ctor     of int * adt_info
  | VI_MethodFn of S.method_name

type t = unit

let empty = ()

let toplevel = ()

(* = {
  var_map  : var_info ident_info StrMap.t;
    (** Information about regular variable names *)

  tvar_map : T.typ ident_info StrMap.t;
    (** Information about named type variables *)

  implicit_map : (T.var * T.scheme * (Position.t -> unit)) ident_info StrMap.t;
    (** Information about named implicits *)

  ctor_map : (int * adt_info) ident_info StrMap.t;
    (** Information about ADT constructors *)

  mod_map : t ident_info StrMap.t;
    (** Information about defined modules *)
}

let unit_info =
  { adt_proof = { T.pos = Position.nowhere; T.data = T.EUnitPrf };
    adt_args  = [];
    adt_ctors =
      [ { ctor_name        = "()";
          ctor_targs       = [];
          ctor_named       = [];
          ctor_arg_schemes = []
        } ];
    adt_type  = T.Type.t_unit;
    adt_strictly_positive = true
  }

let empty =
  { var_map      = StrMap.empty;
    tvar_map     = StrMap.empty;
    implicit_map = StrMap.empty;
    ctor_map     = StrMap.empty;
    mod_map      = StrMap.empty
  }

let toplevel =
  { empty with
    var_map  =
      StrMap.singleton "()" { data = VI_Ctor(0, unit_info); public = false };
    tvar_map =
      T.BuiltinType.all
      |> List.map (fun (name, tv) ->
        (name, { data = T.Type.t_var tv; public = false }))
      |> List.to_seq |> StrMap.of_seq;
    ctor_map = StrMap.singleton "()" { data = (0, unit_info); public = false }
  }
*)
let add_type_alias ~public ~on_use m name tp =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let x = T.TVar.fresh kind in
  { m with
    tvar_map = StrMap.add name { data = (T.Type.t_var x); public } m.tvar_map
  }, x
*)
let add_var ~public ~on_use m x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let y = Var.fresh ~name:x () in
  { m with
    var_map = StrMap.add x { data = VI_Var(y, sch); public } m.var_map
  }, y
*)
let add_implicit ~public ~on_use m name x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let x = Var.fresh ~name () in
  { m with
    implicit_map =
      StrMap.add name { data = (x, sch, on_use); public } m.implicit_map
  }, x
*)
let add_method ~public ~on_use m owner name x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_method_fn ~public ~on_use m x name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { m with
    var_map = StrMap.add x { data = VI_MethodFn name; public } m.var_map
  }
*)
let add_adt ~public m x info =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_ctor ~public m name idx info =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { m with
    var_map  = StrMap.add name { data = VI_Ctor(idx, info); public } m.var_map;
    ctor_map = StrMap.add name { data = (idx, info); public } m.ctor_map
  }
*)
let add_module ~public m name m' =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  { m with
    mod_map = StrMap.add name { data = m'; public } m.mod_map
  }
*)

let open_module ~public m m' =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let combine _ _ { data; _ } = Some { data; public } in
  { var_map      = StrMap.union combine m.var_map      m'.var_map;
    tvar_map     = StrMap.union combine m.tvar_map     m'.tvar_map;
    implicit_map = StrMap.union combine m.implicit_map m'.implicit_map;
    ctor_map     = StrMap.union combine m.ctor_map     m'.ctor_map;
    mod_map      = StrMap.union combine m.mod_map      m'.mod_map
  }
*)
let leave m pp_module =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*  let public _ info = info.public in
  { var_map      = StrMap.filter public m.var_map;
    tvar_map     = StrMap.filter public m.tvar_map;
    implicit_map = StrMap.filter public m.implicit_map;
    ctor_map     = StrMap.filter public m.ctor_map;
    mod_map      = StrMap.filter public m.mod_map
  }
*)
(*
let find_opt_data x m =
  StrMap.find_opt x m |> Option.map (fun i -> i.data)
*)
let lookup_var m x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  find_opt_data x m.var_map
*)
let lookup_tvar m x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  find_opt_data x m.tvar_map
*)
let lookup_implicit m x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  find_opt_data x m.implicit_map
*)
let lookup_method m owner x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let lookup_ctor m name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  find_opt_data name m.ctor_map
*)

let lookup_adt m x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let lookup_module m name =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  find_opt_data name m.mod_map

let rec lookup_path m lookup (p : 'a S.path) =
  match p with
  | NPName x    -> lookup m x
  | NPSel(x, p) ->
    Option.bind (lookup_module m x) (fun m -> lookup_path m lookup p)
*)

let pp_module m =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let public_types m =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let public_vars m =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let public_implicits m =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
