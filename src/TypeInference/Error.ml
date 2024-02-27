(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

type t = Position.t * string * (Position.t * string) list

let report_note (pos, msg) =
  InterpLib.Error.report ~pos ~cls:Note msg

let fatal (pos, msg, notes) =
  InterpLib.Error.report ~pos ~cls:FatalError msg;
  List.iter report_note notes;
  raise InterpLib.Error.Fatal_error

let report (pos, msg, notes) =
  InterpLib.Error.report ~pos ~cls:Error msg;
  List.iter report_note notes

let warn (pos, msg, notes) =
  InterpLib.Error.report ~pos ~cls:Warning msg;
  List.iter report_note notes

let string_of_name (name : Lang.Unif.name) =
  match name with
  | NLabel -> "the effect label"
  | NImplicit n -> Printf.sprintf "implicit parameter %s" n
  | NVar x      -> Printf.sprintf "named parameter %s" x

let kind_mismatch ~pos k1 k2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This type has kind %s, but it was expected of kind %s"
    (Pretty.kind_to_string pp_ctx k1)
    (Pretty.kind_to_string pp_ctx k2)
  in (pos, msg, [])

let wildcard_in_effect ~pos =
  (pos, "Wild-cards in effects are forbidden", [])

let anon_effect_arg ~pos =
  (pos, "Anonymous type parameters cannot have the effect kind", [])

let effect_arg_kind_mismatch ~pos k =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This type parameter has kind %s, but it was expected of the effect kind"
    (Pretty.kind_to_string pp_ctx k)
  in (pos, msg, [])

let type_not_function ~pos k =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This type has kind %s, and cannot be applied"
    (Pretty.kind_to_string pp_ctx k)
  in (pos, msg, [])

let unbound_var ~pos x =
  (pos, Printf.sprintf "Unbound variable %s" x, [])

let unbound_implicit ~pos name =
  (pos, Printf.sprintf "Unbound implicit %s" name, [])

let unbound_constructor ~pos name =
  (pos, Printf.sprintf "Unbound constructor %s" name, [])

let unbound_type_var ~pos x =
  (pos, Printf.sprintf "Unbound type %s" x, [])

let unbound_the_effect ~pos =
  (pos, Printf.sprintf "There is no default effect in this context", [])

let unbound_named_param ~pos x =
  (pos, Printf.sprintf "Cannot implicitly provide a parameter of name %s" x,
    [])

let unbound_the_label ~pos =
  (pos, Printf.sprintf "There is no default label in this context", [])

let unbound_method ~pos ~env x name =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type %s has no method named %s"
    (Pretty.tvar_to_string pp_ctx env x)
    name
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let expr_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s, but an expression was expected of type %s"
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let expr_effect_mismatch ~pos ~env eff1 eff2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has effect %s, but an expression was expected of effect %s"
    (Pretty.type_to_string pp_ctx env eff1)
    (Pretty.type_to_string pp_ctx env eff2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let delim_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("Type mismatch between label and capability: " ^^
    "the label provides %s as a type of a delimiter, " ^^
    "while the capability provides %s")
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let delim_effect_mismatch ~pos ~env eff1 eff2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("Type mismatch between label and capability: " ^^
    "the label provides %s as an effect of a delimiter, " ^^
    "while the capability provides %s")
    (Pretty.type_to_string pp_ctx env eff1)
    (Pretty.type_to_string pp_ctx env eff2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let pattern_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This pattern matches values of type %s, but it was expected to match values of type %s"
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let pattern_annot_mismatch ~pos ~env sch1 sch2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Annotating pattern with type %s, but it was expected to match values of type %s"
    (Pretty.scheme_to_string pp_ctx env sch2)
    (Pretty.scheme_to_string pp_ctx env sch1)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let func_effect_mismatch ~pos ~env eff1 eff2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This function has effect %s, but it is applied in a context that accepts effect %s"
    (Pretty.type_to_string pp_ctx env eff1)
    (Pretty.type_to_string pp_ctx env eff2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_effect_mismatch ~pos ~env eff1 eff2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This method has effect %s, but it is applied in a context that accepts effect %s"
    (Pretty.type_to_string pp_ctx env eff1)
    (Pretty.type_to_string pp_ctx env eff2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let func_not_pure ~pos =
  (pos, "Cannot ensure that this function is pure and always terminates", [])

let impure_handler ~pos =
  (pos,
    "Cannot ensure that this handler expression is pure and always terminates",
    [])

let expr_not_function ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. It is not a function and cannot be applied"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let expr_not_function_ctx ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression should not be a function, the expected type is %s"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let expr_not_handler ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. It cannot be used as a handler"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let expr_not_handler_ctx ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression should not be a handler, the expected type is %s"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let expr_not_label ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. It cannot be used as a label"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_call_on_unknown_type ~pos =
  (pos, "Calling method of expression of unknown type", [])

let method_call_on_invalid_type ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. This type cannot have any methods"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_of_bound_tvar ~pos ~env sch =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Self type is not free"
    (Pretty.scheme_to_string pp_ctx env sch)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_of_unknown_type ~pos ~env sch =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Self type is unknown here"
    (Pretty.scheme_to_string pp_ctx env sch)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_of_invalid_type ~pos ~env sch tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Type %s cannot have any methods"
    (Pretty.scheme_to_string pp_ctx env sch)
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_of_polymorphic_type ~pos ~env sch =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Self cannot be polymorphic"
    (Pretty.scheme_to_string pp_ctx env sch)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let non_arrow_method ~pos ~env sch =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. This type is not an arrow"
    (Pretty.scheme_to_string pp_ctx env sch)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let ctor_pattern_on_non_adt ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This pattern matches values of type %s, which is not an ADT"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let empty_match_on_non_adt ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This pattern matching matches values of type %s, which is not an ADT"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let empty_match_on_nonempty_adt ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This pattern matching matches values of type %s, which is not an empty ADT"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let ctor_not_in_type ~pos ~env name tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "There is no constructor named %s in type %s"
    name
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let type_escapes_its_scope ~pos ~env x =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type variable %s escapes its scope"
    (Pretty.tvar_to_string pp_ctx env x)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let cannot_guess_effect_param ~pos (name : Lang.Unif.tname) =
  (pos,
    Printf.sprintf "Cannot guess effect named %s"
      (Pretty.tname_to_string name),
    [])

let cannot_guess_label_effect ~pos =
  (pos, "Cannot guess the effect of this label", [])

let ungeneralizable_implicit ~pos name =
  (pos, Printf.sprintf "Implicit %s is used, but cannot be generalized" name,
    [])

let non_polymorphic_pattern ~pos =
  (pos, Printf.sprintf "This pattern cannot match polymorphic values", [])

let polymorphic_label ~pos =
  (pos, "Labels cannot be polymorphic", [])

let label_type_mismatch ~pos =
  (pos, "Labels cannot have non-label type", [])

let label_pattern_type_mismatch ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This label pattern is expected of type %s"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let looping_named_param ~pos name =
  (pos,
    Printf.sprintf "Resolving of %s leads to an infinite loop"
      (string_of_name name),
    [])

let named_param_type_mismatch ~pos ~env name tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type error during resolving of implicit parameters: %s has type %s, but the expected type is %s"
    (string_of_name name)
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let ctor_redefinition ~pos ~ppos name =
  (pos, Printf.sprintf "Constructor %s is defined more than once" name,
    [ (ppos, "Here is a previous definition") ])

let type_inst_redefinition ~pos ~ppos (name : Lang.Surface.tname) =
  (pos,
    Printf.sprintf "Type %s is provided more than once"
      (Pretty.tname_to_string (Name.tr_tname name)),
    [ ppos, "Here is a previous definition" ])

let inst_redefinition ~pos ~ppos (name : Lang.Surface.name) =
  let nn =
    match name with
    | NLabel      -> Printf.sprintf "The label"
    | NImplicit n -> Printf.sprintf "Implicit parameter %s" n
    | NVar      x -> Printf.sprintf "Named parameter %s" x
  in
  (pos, Printf.sprintf "%s is provided more than once" nn,
    [ ppos, "Here is a previous definition" ])

let multiple_named_type_args ~pos ~ppos (name : Lang.Surface.tname) =
  (pos,
    Printf.sprintf "Named type %s is bound more than once in single definition"
      (Pretty.tname_to_string (Name.tr_tname name)),
    [ ppos, "Here is a previous type binder with this name" ])

let ctor_type_arg_same_as_data_arg ~pos (name : Lang.Surface.tname) =
  (pos,
    Printf.sprintf "Named type %s is already bound by datatype itself"
      (Pretty.tname_to_string (Name.tr_tname name)), [])

let multiple_inst_patterns ~pos ~ppos (name : Lang.Surface.name) =
  let nn =
    match name with
    | NLabel      -> Printf.sprintf "The label"
    | NImplicit n -> Printf.sprintf "Implicit parameter %s" n
    | NVar      x -> Printf.sprintf "Named parameter %s" x
  in
  (pos,
    Printf.sprintf "%s is provided more than once" nn,
    [ ppos, "Here is a previous parameter with this name" ])

let multiple_name_binders ~pos1 ~pos2 name =
  (pos2, Printf.sprintf "%s is bound more than once in the same pattern"
    (string_of_name name),
    [ pos1, "Here is a previous binding" ])

let type_generalized_twice ~pos name =
  let msg =
    Printf.sprintf "Type %s is generalized twice"
      (Pretty.tname_to_string name) in
  (pos, msg, [])

let ctor_arity_mismatch ~pos cname req_n prov_n =
  (pos,
    Printf.sprintf "Constructor %s expects %d parameter(s), but is applied to %d"
      cname req_n prov_n,
    [])

let redundant_named_type ~pos (name : Lang.Unif.tname) =
  let nn =
    match name with
    | TNEffect -> "the effect"
    | TNAnon   -> assert false
    | TNVar x  -> Printf.sprintf "type %s" x
  in
  (pos, Printf.sprintf "Providing %s to a function that do not expect it" nn, [])

let redundant_named_parameter ~pos name =
  (pos,
    Printf.sprintf "Providing %s to a function that do not expect it"
      (string_of_name name),
    [])

let redundant_named_pattern ~pos name =
  (pos, Printf.sprintf
    "Providing %s to a constructor that do not expect it"
    (string_of_name name),
    [])
