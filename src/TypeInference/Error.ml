(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

open Common

type t = Position.t * string * (Position.t * string) list

let report_note (pos, msg) =
  InterpLib.Error.report ~pos ~cls:Note msg

let add_notes (pos, msg, notes) new_notes =
  (pos, msg, notes @ new_notes)

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

let rec string_of_path (p : string S.path) =
  match p.data with
  | NPName x    -> x
  | NPSel(p, n) -> Printf.sprintf "%s.%s" (string_of_path p) n

let string_of_name (name : T.name) =
  match name with
  | NImplicit n    -> Printf.sprintf "implicit parameter %s" n
  | NVar x         -> Printf.sprintf "named parameter %s" x
  | NOptionalVar x -> Printf.sprintf "optional named parameter %s" x
  | NMethod n      -> Printf.sprintf "method %s" n

let escaping_tvar_message ~env x =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type variable %s escapes its scope"
    (Pretty.tvar_to_string pp_ctx env x)
  in msg ^ Pretty.additional_info pp_ctx

let unification_error_to_string (err : Unification.error_info) =
  match err with 
  | TVarEscapesScope(env, tv) -> escaping_tvar_message ~env tv

let check_unify_result ?(is_fatal=false) ~pos
    (result : Unification.result) ~on_error =
  let inform = if is_fatal then fatal else report in
  match result with
  | Unify_Success -> ()
  | Unify_Fail errors -> 
    inform (add_notes (on_error ~pos)
      (List.map (fun err -> (pos, unification_error_to_string err)) errors))
(*
let kind_mismatch ~pos k1 k2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This type has kind %s, but it was expected of kind %s"
    (Pretty.kind_to_string pp_ctx k1)
    (Pretty.kind_to_string pp_ctx k2)
  in (pos, msg, [])
*)
let named_type_kind_mismatch ~pos name k1 k2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type %s provided here has kind %s, but it was expected of kind %s"
    name
    (Pretty.kind_to_string pp_ctx k1)
    (Pretty.kind_to_string pp_ctx k2)
  in (pos, msg, [])

let kind_annot_mismatch ~pos k k_annot = 
  let pp_ctx = Pretty.empty_context () in 
  let msg = Printf.sprintf
    "Kind %s of type differs from its annotation %s"
    (Pretty.kind_to_string pp_ctx k)
    (Pretty.kind_to_string pp_ctx k_annot)
  in (pos, msg, [])
(*
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
  (pos, Printf.sprintf "Unbound variable %s" (string_of_path x), [])

let unbound_implicit ~pos name =
  (pos, Printf.sprintf "Unbound implicit %s" (string_of_path name), [])

let unbound_constructor ~pos name =
  (pos, Printf.sprintf "Unbound constructor %s" (string_of_path name), [])

let unbound_type_var ~pos x =
  (pos, Printf.sprintf "Unbound type %s" (string_of_path x), [])

let unbound_module ~pos name =
  (pos, Printf.sprintf "Unbound module %s" (string_of_path name), [])

let unbound_the_effect ~pos =
  (pos, Printf.sprintf "There is no default effect in this context", [])
*)
let unbound_the_label ~pos =
  (pos, Printf.sprintf "There is no default label in this context", [])

let unbound_method ~pos ~env x name =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type %s has no method named %s"
    (Pretty.tvar_to_string pp_ctx env x)
    name
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
(*
let unbound_adt ~pos ~env x =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "There is no ADT assigned to this type var %s"
    (Pretty.tvar_to_string pp_ctx env x)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
*)
let method_fn_without_arg ~pos x name =
  (pos, Printf.sprintf
    ("Variable %s is registered as method %s"
    ^^ " and cannot be used without argument")
    (string_of_path x) name, [])

let expr_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s, but an expression was expected of type %s"
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
(*
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
*)
let pattern_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("This pattern matches values of type %s,"
    ^^ " but it was expected to match values of type %s")
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let pattern_annot_mismatch ~pos ~env sch1 sch2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("Annotating pattern with type %s, but"
    ^^ " it was expected to match values of type %s")
    (Pretty.scheme_to_string pp_ctx env sch2)
    (Pretty.scheme_to_string pp_ctx env sch1)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
(*
let func_effect_mismatch ~pos ~env eff1 eff2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("This function has effect %s, but"
    ^^ " it is applied in a context that accepts effect %s")
    (Pretty.type_to_string pp_ctx env eff1)
    (Pretty.type_to_string pp_ctx env eff2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let method_effect_mismatch ~pos ~env eff1 eff2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("This method has effect %s, but it is applied"
    ^^ " in a context that accepts effect %s")
    (Pretty.type_to_string pp_ctx env eff1)
    (Pretty.type_to_string pp_ctx env eff2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
*)
let return_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Return clause has type %s, but was expected to have type %s"
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let finally_type_mismatch ~pos ~env tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Finally clause has type %s, but was expected to have type %s"
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let func_not_pure ~pos =
  (pos, "Cannot ensure that this function is pure and always terminates", [])

let impure_handler ~pos =
  (pos,
    "Cannot ensure that this handler expression is pure and always terminates",
    [])
(*
let invalid_rec_def ~pos =
  (pos, "This kind of definition cannot be recursive", [])

let non_productive_rec_def ~pos =
  (pos, "Non-productive recursive definition", [])
*)
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

let wrong_label_type ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "The implicit label has type %s. It cannot be used as a label"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

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
    ("This pattern matching matches values of type %s,"
    ^^ " which is not an empty ADT")
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let ctor_not_in_type ~pos ~env name tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "There is no constructor named %s in type %s"
    name
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

(*
let type_escapes_its_scope ~pos ~env x =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type variable %s escapes its scope"
    (Pretty.tvar_to_string pp_ctx env x)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
*)

(*
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
*)
let non_polymorphic_pattern ~pos =
  (pos, Printf.sprintf "This pattern cannot match polymorphic values", [])
(*
let polymorphic_label ~pos =
  (pos, "Labels cannot be polymorphic", [])

let polymorphic_optional_parameter ~pos =
  (pos, "Optional parameters cannot be polymorphic", [])

let label_type_mismatch ~pos =
  (pos, "Labels cannot have non-label type", [])

let label_pattern_type_mismatch ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "This label pattern is expected of type %s"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
*)
let anonymous_type_pattern ~pos =
  (pos, "Anonymous types patterns cannot be used in scheme-checking mode", [])

let looping_named_param ~pos name =
  (pos,
    Printf.sprintf "Resolving of %s leads to an infinite loop"
      (string_of_name name),
    [])

let named_param_type_mismatch ~pos ~env name tp1 tp2 =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    ("Type error during resolving of named "
    ^^ "parameters: %s has type %s, but the expected type is %s")
    (string_of_name name)
    (Pretty.type_to_string pp_ctx env tp1)
    (Pretty.type_to_string pp_ctx env tp2)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let ctor_redefinition ~pos ~ppos name =
  (pos, Printf.sprintf "Constructor %s is defined more than once" name,
    [ (ppos, "Here is a previous definition") ])

let cannot_resolve_named_param ~pos x =
  (pos, Printf.sprintf "Cannot resolve a parameter of name %s" x, [])

let cannot_resolve_implicit ~pos name =
  (pos, Printf.sprintf "Cannot resolve an implicit parameter %s" name, [])

let cannot_resolve_method ~pos env owner name =
  let pp_ctx = Pretty.empty_context () in
  let msg =
    Printf.sprintf
      "Cannot resolve a method %s that belongs to %s" name
      (Pretty.tvar_to_string pp_ctx env owner)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])

let type_already_provided ~pos ~npos name =
  (pos,
    Printf.sprintf "Type %s is provided more than once" name,
    [ npos, "Here is the last definition" ])

let named_param_already_provided ~pos ~npos (name : T.name) =
  let nn =
    match name with
    | NImplicit    n -> Printf.sprintf "Implicit parameter %s" n
    | NVar         x -> Printf.sprintf "Named parameter %s" x
    | NOptionalVar x -> Printf.sprintf "Optional named parameter %s" x
    | NMethod      n -> Printf.sprintf "Method %s" n
  in
  (pos, Printf.sprintf "%s is provided more than once" nn,
    [ npos, "Here is the last definition" ])

let named_param_provided_as_optional ~pos name =
  (pos, Printf.sprintf "Named parameter %s is provided as optional" name, [])

let method_instantiation_not_allowed ~pos =
  (pos, "Method instantiation is not allowed", [])

let unknown_named_type_pattern ~pos name =
  (pos, Printf.sprintf "Type %s was not expected here" name, [])

let unknown_named_pattern ~pos name =
  (pos,
    Printf.sprintf "A pattern for %s was not expected here"
      (string_of_name name),
    [])

let multiple_named_patterns ~pos ~ppos name =
  (pos,
    Printf.sprintf "Multiple patterns for %s" (string_of_name name),
    [ ppos, "Here is a previous pattern" ])

let method_pattern_not_allowed ~pos =
  (pos, "Method patterns are not allowed", [])

let open_pattern_not_allowed ~pos =
  (pos, "Opening patterns are not allowed in the inference mode", [])

let multiple_named_type_args ~pos ~ppos (name : S.tvar) =
  (pos,
    Printf.sprintf "Named type %s is bound more than once in single definition"
      name,
    [ ppos, "Here is a previous type binder with this name" ])

let multiple_named_args ~pos ~ppos (name : T.name) =
  let nn =
    match name with
    | NImplicit    n -> Printf.sprintf "Implicit parameter %s" n
    | NVar         x -> Printf.sprintf "Named parameter %s" x
    | NOptionalVar x -> Printf.sprintf "Optional named parameter %s" x
    | NMethod      n -> Printf.sprintf "Method %s" n
  in
  (pos, Printf.sprintf "%s is bound more than once" nn,
    [ ppos, "Here is a previous parameter with this name" ])

let multiple_method_args ~env ~pos ~ppos owner name =
  let pp_ctx = Pretty.empty_context () in
  let msg =
    Printf.sprintf
      "Method %s that belongs to type %s is bound more than once"
      name
      (Pretty.tvar_to_string pp_ctx env owner)
  in (pos, msg ^ Pretty.additional_info pp_ctx,
    [ ppos, "Here is a previous method with this name" ])

let ctor_type_arg_same_as_data_arg ~pos (name : S.tvar) =
  (pos,
    Printf.sprintf "Named type %s is already bound by datatype itself"
      name, [])
(*
let multiple_inst_patterns ~pos ~ppos (name : Lang.Surface.name) =
  let nn =
    match name with
    | NLabel         -> Printf.sprintf "The label"
    | NImplicit    n -> Printf.sprintf "Implicit parameter %s" n
    | NVar         x -> Printf.sprintf "Named parameter %s" x
    | NOptionalVar x -> Printf.sprintf "Optional named parameter %s" x
    | NMethod      n -> Printf.sprintf "Method %s" n
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
*)
let ctor_arity_mismatch ~pos cpath req_n prov_n =
  (pos,
    Printf.sprintf
      "Constructor %s expects %d parameter(s), but is applied to %d"
      (string_of_path cpath) req_n prov_n,
    [])

let redundant_named_type ~pos name =
  (pos, Printf.sprintf
      "Providing type %s to a function that do not expect it" name, [])

let redundant_named_parameter ~pos name =
  (pos,
    Printf.sprintf "Providing %s to a function that do not expect it"
      (string_of_name name),
    [])
(*
let redundant_named_pattern ~pos name =
  (pos, Printf.sprintf
    "Providing %s to a constructor that do not expect it"
    (string_of_name name),
    [])

let invalid_whnf_form ~pos ~env tp =
  let pp_ctx = Pretty.empty_context () in
  let msg = Printf.sprintf
    "Got invalid whnf form when converting type %s"
    (Pretty.type_to_string pp_ctx env tp)
  in (pos, msg ^ Pretty.additional_info pp_ctx, [])
*)
