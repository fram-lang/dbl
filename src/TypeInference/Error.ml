(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

type t = unit

let fatal () =
  InterpLib.Error.incr_error_counter ();
  raise InterpLib.Error.Fatal_error

let report () =
  InterpLib.Error.incr_error_counter ()

let warn () = ()

let kind_mismatch ~pos k1 k2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Kind mismatch\n"
    (Position.to_string pos)

let type_not_function ~pos ~env k =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: This type cannot be applied.\n"
    (Position.to_string pos)

let unbound_var ~pos x =
  Printf.eprintf "%s: error: Unbound variable `%s'\n"
    (Position.to_string pos) x

let unbound_implicit ~pos name =
  Printf.eprintf "%s: error: Unbound implicit %s\n"
    (Position.to_string pos) name

let unbound_constructor ~pos name =
  Printf.eprintf "%s: error: Unbound constructor %s\n"
    (Position.to_string pos) name

let unbound_type_var ~pos x =
  Printf.eprintf "%s: error: Unbound type %s\n"
    (Position.to_string pos) x

let unbound_named_param ~pos x =
  Printf.eprintf "%s: error: Cannot implicitly provide a parameter of name %s\n"
    (Position.to_string pos) x

let expr_type_mismatch ~pos ~env tp1 tp2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Type mismatch\n"
    (Position.to_string pos)

let expr_effect_mismatch ~pos ~env eff1 eff2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Effect mismatch\n"
    (Position.to_string pos)

let pattern_type_mismatch ~pos ~env tp1 tp2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Type mismatch\n"
    (Position.to_string pos)

let pattern_annot_mismatch ~pos ~env sch1 sch2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Mismatched scheme annotation of the pattern\n"
    (Position.to_string pos)

let func_effect_mismatch ~pos ~env eff1 eff2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Function effect mismatch\n"
    (Position.to_string pos)

let func_not_pure ~pos =
  Printf.eprintf
    "%s: error: Cannot ensure that this function is pure and always terminates.\n"
    (Position.to_string pos)

let expr_not_function ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: This expression is not a function and cannot be applied.\n"
    (Position.to_string pos)

let expr_not_function_ctx ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf 
    "%s: error: This expresion should not be a function.\n"
    (* the expected type is tp *)
    (Position.to_string pos)

let type_escapes_its_scope ~pos ~env x =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Something escapes its scope here.\n"
    (Position.to_string pos)

let ungeneralizable_implicit ~pos name =
  Printf.eprintf
    "%s: error: Implicit %s is used, but cannot be generalized\n"
    (Position.to_string pos)
    name

let non_polymorphic_pattern ~pos =
  Printf.eprintf
    "%s: error: This pattern cannot match polymorphic values.\n"
    (Position.to_string pos)

let looping_named_param ~pos (name : Lang.Unif.name) =
  let nn =
    match name with
    | NVar x -> Printf.sprintf "named parameter %s" x
    | NImplicit n -> Printf.sprintf "implicit %s" n
  in
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Resolving of %s leads to an infinite loop.\n"
    (Position.to_string pos)
    nn

let implicit_type_mismatch ~pos ~env name tp1 tp2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Type mismatch of implicit %s\n"
    (Position.to_string pos)
    name

let ctor_redefinition ~pos ~ppos name =
  Printf.eprintf "%s: error: Constructor %s is defined more than once.\n"
    (Position.to_string pos)
    name;
  Printf.eprintf "%s: note: Here is a previous definition.\n"
    (Position.to_string ppos)

let inst_redefinition ~pos ~ppos (name : Lang.Surface.name) =
  let nn =
    match name with
    | NImplicit n -> Printf.sprintf "Implicit parameter %s" n
    | NVar      x -> Printf.sprintf "Named parameter %s" x
  in
  Printf.eprintf "%s: error: %s is provided more than once.\n"
    (Position.to_string pos)
    nn;
  Printf.eprintf "%s: note: Here is a previous definition.\n"
    (Position.to_string ppos)

let multiple_inst_patterns ~pos ~ppos (name : Lang.Surface.name) =
  let nn =
    match name with
    | NImplicit n -> Printf.sprintf "Implicit parameter %s" n
    | NVar      x -> Printf.sprintf "Named parameter %s" x
  in
  Printf.eprintf "%s: error: %s is provided more than once.\n"
    (Position.to_string pos)
    nn;
  Printf.eprintf "%s: note: Here is a previous parameter with this name.\n"
    (Position.to_string ppos)

let multiple_name_binders ~pos1 ~pos2 (name : Lang.Unif.name) =
  let nn =
    match name with
    | NImplicit n -> Printf.sprintf "Implicit %s" n
    | NVar      x -> Printf.sprintf "Variable %s" x
  in
  Printf.eprintf "%s: error: %s is bound more than once in the same pattern.\n"
    (Position.to_string pos2)
    nn;
  Printf.eprintf "%s: note: Here is a previous binding.\n"
    (Position.to_string pos1)

let ctor_arity_mismatch ~pos cname req_n prov_n =
  Printf.eprintf
    "%s: error: Constructor %s expects %d parameter(s), but is applied to %d.\n"
    (Position.to_string pos)
    cname req_n prov_n

let redundant_named_parameter ~pos (name : Lang.Unif.name) =
  let nn =
    match name with
    | NImplicit n -> Printf.sprintf "implicit parameter %s" n
    | NVar      x -> Printf.sprintf "named parameter %s" x
  in
  Printf.eprintf
    "%s: warning: Providing %s to a function that do not expect it.\n"
    (Position.to_string pos)
    nn

let redundant_named_pattern ~pos (name : Lang.Unif.name) =
  let nn =
    match name with
    | NImplicit n -> Printf.sprintf "implicit parameter %s" n
    | NVar      x -> Printf.sprintf "named parameter %s" x
  in
  Printf.eprintf
    "%s: warning: Providing %s to a constructor that do not expect it.\n"
    (Position.to_string pos)
    nn
