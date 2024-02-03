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

let string_of_name (name : Lang.Unif.name) =
  match name with
  | NLabel -> "the effect label"
  | NImplicit n -> Printf.sprintf "implicit parameter %s" n
  | NVar x      -> Printf.sprintf "named parameter %s" x

let kind_mismatch ~pos k1 k2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Kind mismatch\n"
    (Position.to_string pos)

let wildcard_in_effect ~pos =
  Printf.eprintf "%s: error: Wild-cards in effects are forbidden\n"
    (Position.to_string pos)

let anon_effect_arg ~pos =
  Printf.eprintf "%s: error: Anonymous type parameters cannot have effect kind\n"
    (Position.to_string pos)

let effect_arg_kind_mismatch ~pos k =
  Printf.eprintf "%s: error: This type parameter should have effect kind\n"
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

let unbound_the_label ~pos =
  Printf.eprintf "%s: error: There is no default label in this context\n"
    (Position.to_string pos)

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

let impure_handler ~pos =
  Printf.eprintf
    "%s: error: Cannot ensure that this handler expression is pure and always terminates.\n"
    (Position.to_string pos)

let expr_not_function ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: This expression is not a function and cannot be applied.\n"
    (Position.to_string pos)

let expr_not_function_ctx ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf 
    "%s: error: This expression should not be a function.\n"
    (* the expected type is tp *)
    (Position.to_string pos)

let expr_not_handler ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: This expression is not a handler.\n"
    (Position.to_string pos)

let expr_not_handler_ctx ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: This expression should not be a handler.\n"
    (Position.to_string pos)

let empty_match_on_non_adt ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Empty pattern-matching on a type which is not known to be an ADT.\n"
    (Position.to_string pos)

let empty_match_on_nonempty_adt ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Empty pattern-matching on a type which is not known to be an empty ADT.\n"
    (Position.to_string pos)

let type_escapes_its_scope ~pos ~env x =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Something escapes its scope here.\n"
    (Position.to_string pos)

let cannot_guess_effect_param ~pos (name : Lang.Unif.tname) =
  let nn =
    match name with
    | TNAnon   -> assert false
    | TNEffect -> "effect"
    | TNVar x  -> x
  in
  Printf.eprintf
    "%s: error: Cannot guess effect named %s\n"
      (Position.to_string pos)
      nn

let ungeneralizable_implicit ~pos name =
  Printf.eprintf
    "%s: error: Implicit %s is used, but cannot be generalized\n"
    (Position.to_string pos)
    name

let non_polymorphic_pattern ~pos =
  Printf.eprintf
    "%s: error: This pattern cannot match polymorphic values.\n"
    (Position.to_string pos)

let looping_named_param ~pos name =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Resolving of %s leads to an infinite loop.\n"
    (Position.to_string pos)
    (string_of_name name)

let named_param_type_mismatch ~pos ~env name tp1 tp2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Type mismatch of %s\n"
    (Position.to_string pos)
    (string_of_name name)

let ctor_redefinition ~pos ~ppos name =
  Printf.eprintf "%s: error: Constructor %s is defined more than once.\n"
    (Position.to_string pos)
    name;
  Printf.eprintf "%s: note: Here is a previous definition.\n"
    (Position.to_string ppos)

let type_inst_redefinition ~pos ~ppos (name : Lang.Surface.tname) =
  let nn =
    match name with
    | TNAnon   -> assert false
    | TNEffect -> "effect"
    | TNVar x  -> x
  in
  Printf.eprintf "%s: error: Type %s is provided more than once.\n"
    (Position.to_string pos)
    nn;
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

let multiple_named_type_args ~pos ~ppos (name : Lang.Surface.tname) =
  let nn =
    match name with
    | TNAnon   -> assert false
    | TNEffect -> "effect"
    | TNVar x  -> x
  in
  Printf.eprintf
    "%s: error: Named type %s is bound more than once in single definition.\n"
    (Position.to_string pos)
    nn;
  Printf.eprintf "%s: note: Here is a previous type binder with this name.\n"
    (Position.to_string ppos)

let ctor_type_arg_same_as_data_arg ~pos (name : Lang.Surface.tname) =
  let nn =
    match name with
    | TNAnon   -> assert false
    | TNEffect -> "effect"
    | TNVar x  -> x
  in
  Printf.eprintf
    "%s: error: Named type %s is already bound by datatype itself.\n"
    (Position.to_string pos)
    nn

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

let multiple_name_binders ~pos1 ~pos2 name =
  Printf.eprintf "%s: error: %s is bound more than once in the same pattern.\n"
    (Position.to_string pos2)
    (string_of_name name);
  Printf.eprintf "%s: note: Here is a previous binding.\n"
    (Position.to_string pos1)

let ctor_arity_mismatch ~pos cname req_n prov_n =
  Printf.eprintf
    "%s: error: Constructor %s expects %d parameter(s), but is applied to %d.\n"
    (Position.to_string pos)
    cname req_n prov_n

let redundant_named_type ~pos (name : Lang.Unif.tname) =
  let nn =
    match name with
    | TNEffect -> "the effect"
    | TNAnon   -> assert false
    | TNVar x  -> Printf.sprintf "type %s" x
  in
  Printf.eprintf
    "%s: warning: Providing %s to a function that do not expect it.\n"
    (Position.to_string pos)
    nn

let redundant_named_parameter ~pos name =
  Printf.eprintf
    "%s: warning: Providing %s to a function that do not expect it.\n"
    (Position.to_string pos)
    (string_of_name name)

let redundant_named_pattern ~pos name =
  Printf.eprintf
    "%s: warning: Providing %s to a constructor that do not expect it.\n"
    (Position.to_string pos)
    (string_of_name name)
