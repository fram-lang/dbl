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

let string_of_method_owner ~pp_ctx ~pp ?(cap=false) (own : Name.method_owner) =
  match own with
  | MO_Arrow   -> if cap then "The arrow scope" else "the arrow scope"
  | MO_Handler -> if cap then "The handler scope" else "the handler scope"
  | MO_Label   -> if cap then "The label scope" else "the label scope"
  | MO_TVar x  ->
    Printf.sprintf "%cype %s"
      (if cap then 'T' else 't')
      (T.Pretty.pp_tvar pp_ctx pp x)

let string_of_name ~pp ~pp_ctx ?(cap=false) (name : Name.t) =
  match name with
  | NVar x         ->
    Printf.sprintf "%camed parameter %s" (if cap then 'N' else 'n') x
  | NOptionalVar x ->
    Printf.sprintf "%cptional parameter %s" (if cap then 'O' else 'o') x
  | NImplicit n    ->
    Printf.sprintf "%cmplicit parameter %s" (if cap then 'I' else 'i') n
  | NMethod(own, n)      ->
    Printf.sprintf "%cethod %s associated with %s"
      (if cap then 'M' else 'm') n (string_of_method_owner ~pp ~pp_ctx own)

let string_of_val_name ~pp ~pp_ctx ?(cap=false) (name : Name.t) =
  match name with
  | NVar x | NOptionalVar x ->
    Printf.sprintf "%cariable %s" (if cap then 'V' else 'v') x
  | NImplicit n ->
    Printf.sprintf "%cmplicit %s" (if cap then 'I' else 'i') n
  | NMethod(own, n) ->
    Printf.sprintf "%cethod %s associated with %s"
      (if cap then 'M' else 'm') n (string_of_method_owner ~pp ~pp_ctx own)

let escaping_tvar_message ~env x =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type variable %s escapes its scope"
    (T.Pretty.pp_tvar pp_ctx env x)
  in msg ^ T.Pretty.additional_info pp_ctx

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

let kind_mismatch ~pos k1 k2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This type has kind %s, but it was expected of kind %s"
    (T.Pretty.pp_kind pp_ctx k1)
    (T.Pretty.pp_kind pp_ctx k2)
  in (pos, msg, [])

let named_type_kind_mismatch ~pos name k1 k2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Type %s provided here has kind %s, but it was expected of kind %s"
    name
    (T.Pretty.pp_kind pp_ctx k1)
    (T.Pretty.pp_kind pp_ctx k2)
  in (pos, msg, [])

let kind_annot_mismatch ~pos k k_annot = 
  let pp_ctx = T.Pretty.empty_context () in 
  let msg = Printf.sprintf
    "Kind %s of type differs from its annotation %s"
    (T.Pretty.pp_kind pp_ctx k)
    (T.Pretty.pp_kind pp_ctx k_annot)
  in (pos, msg, [])

let type_not_function ~pos k =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This type has kind %s, and cannot be applied"
    (T.Pretty.pp_kind pp_ctx k)
  in (pos, msg, [])

let unbound_var (path : S.var S.path) =
  (path.pos, Printf.sprintf "Unbound variable %s" (string_of_path path), [])

let unbound_implicit (path : S.iname S.path) =
  (path.pos, Printf.sprintf "Unbound implicit %s" (string_of_path path), [])

let unbound_ctor (path : S.ctor_name S.path) =
  (path.pos, Printf.sprintf "Unbound constructor %s" (string_of_path path), [])

let unbound_type (path : S.tvar S.path) =
  (path.pos, Printf.sprintf "Unbound type %s" (string_of_path path), [])

let unbound_module (path : S.module_name S.path) =
  (path.pos, Printf.sprintf "Unbound module %s" (string_of_path path), [])

let unbound_the_label ~pos =
  (pos, Printf.sprintf "There is no default label in this context", [])

let unbound_method ~pos ~pp owner name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "%s has no method named %s"
    (string_of_method_owner ~pp_ctx ~pp ~cap:true owner)
    name
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let expr_type_mismatch ~pos ~pp tp1 tp2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s, but an expression was expected of type %s"
    (T.Pretty.pp_type pp_ctx pp tp1)
    (T.Pretty.pp_type pp_ctx pp tp2)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let pattern_type_mismatch ~pos ~pp tp1 tp2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    ("This pattern matches values of type %s,"
    ^^ " but it was expected to match values of type %s")
    (T.Pretty.pp_type pp_ctx pp tp1)
    (T.Pretty.pp_type pp_ctx pp tp2)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let pattern_annot_mismatch ~pos ~pp sch1 sch2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    ("Annotating pattern with type %s, but"
    ^^ " it was expected to match values of type %s")
    (T.Pretty.pp_scheme pp_ctx pp sch2)
    (T.Pretty.pp_scheme pp_ctx pp sch1)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let return_type_mismatch ~pos ~pp tp1 tp2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Return clause has type %s, but was expected to have type %s"
    (T.Pretty.pp_type pp_ctx pp tp1)
    (T.Pretty.pp_type pp_ctx pp tp2)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let finally_type_mismatch ~pos ~pp tp1 tp2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Finally clause has type %s, but was expected to have type %s"
    (T.Pretty.pp_type pp_ctx pp tp1)
    (T.Pretty.pp_type pp_ctx pp tp2)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let repl_to_string_type_mismatch ~pos ~pp ~self_tp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    ("This expression has type %s. For this type the `toString` method "
    ^^ "returns type %s, but the String type was expected.")
    (T.Pretty.pp_type pp_ctx pp self_tp)
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let func_not_pure ~pos =
  (pos, "Cannot ensure that this function is pure and always terminates", [])

let impure_handler ~pos =
  (pos,
    "Cannot ensure that this handler expression is pure and always terminates",
    [])

let invalid_rec_def ~pos =
  (pos, "This kind of definition cannot be recursive", [])

let non_productive_rec_def ~pos =
  (pos, "Non-productive recursive definition", [])

let expr_not_function ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. It is not a function and cannot be applied"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let expr_not_function_ctx ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression should not be a function, the expected type is %s"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let expr_not_handler ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. It cannot be used as a handler"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let expr_not_handler_ctx ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression should not be a handler, the expected type is %s"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let expr_not_label ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This expression has type %s. It cannot be used as a label"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let wrong_label_type ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "The implicit label has type %s. It cannot be used as a label"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let polymorphic_label ~pos =
  (pos, "Labels cannot be polymorphic", [])

let method_of_bound_tvar ~pos ~pp sch =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Self type is not free"
    (T.Pretty.pp_scheme pp_ctx pp sch)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let method_of_unknown_type ~pos ~pp sch =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Self type is unknown here"
    (T.Pretty.pp_scheme pp_ctx pp sch)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let method_of_polymorphic_type ~pos ~pp sch =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. Self cannot be polymorphic"
    (T.Pretty.pp_scheme pp_ctx pp sch)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let non_arrow_method ~pos ~pp sch =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot define a method of type %s. This type is not an arrow"
    (T.Pretty.pp_scheme pp_ctx pp sch)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let method_call_on_unknown_type ~pos =
  (pos, "Cannot call a method on an unknown type", [])

let ctor_pattern_on_non_adt ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This pattern matches values of type %s, which is not an ADT"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let empty_match_on_non_adt ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "This pattern matching matches values of type %s, which is not an ADT"
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let empty_match_on_nonempty_adt ~pos ~pp tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    ("This pattern matching matches values of type %s,"
    ^^ " which is not an empty ADT")
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let ctor_not_in_type ~pos ~pp name tp =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "There is no constructor named %s in type %s"
    name
    (T.Pretty.pp_type pp_ctx pp tp)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let ungeneralizable_type_param ~pos ~decl_pos (name : T.tname) =
  let name =
    match name with
    | TNAnon  -> "Anonymous type parameter"
    | TNVar x -> Printf.sprintf "Type parameter %s" x
  in
  (pos,
    Printf.sprintf
      "%s is used, but cannot be generalized"
      name,
    [ decl_pos, "Here is the declaration of this type parameter" ])

let ungeneralizable_named_param ~pos ~decl_pos ~pp (name : Name.t) =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "%s is used, but cannot be generalized"
    (string_of_name ~pp ~pp_ctx ~cap:true name)
  in
  (pos, msg ^ T.Pretty.additional_info pp_ctx,
    [ decl_pos, "Here is the declaration of this parameter" ])

let rejected_type_param_used ~pos ~decl_pos (name : T.tname) =
  let name =
    match name with
    | TNAnon  -> "Anonymous type parameter"
    | TNVar x -> Printf.sprintf "Type parameter %s" x
  in
  (pos,
    Printf.sprintf
      "%s was not generalized, but later used during constraint solving"
      name,
    [ decl_pos, "Here is the declaration of this type parameter" ])

let rejected_named_param_used ~pos ~decl_pos ~pp (name : Name.t) =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "%s was not generalized, but later used during constraint solving"
    (string_of_name ~pp ~pp_ctx ~cap:true name)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx,
    [ decl_pos, "Here is the declaration of this parameter" ])

let non_polymorphic_pattern ~pos =
  (pos, Printf.sprintf "This pattern cannot match polymorphic values", [])

let polymorphic_optional_parameter ~pos =
  (pos, "Optional parameters cannot be polymorphic", [])

let anonymous_type_pattern ~pos =
  (pos, "Anonymous types patterns cannot be used in scheme-checking mode", [])

let looping_named_param ~pos ~pp (name : Name.t) =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Resolving of %s leads to an infinite loop"
    (string_of_name ~pp ~pp_ctx name)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let named_param_type_mismatch ~pos ~pp name tp1 tp2 =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    ("Type error during resolving of named "
    ^^ "parameters: %s has type %s, but the expected type is %s")
    (string_of_name ~pp ~pp_ctx name)
    (T.Pretty.pp_type pp_ctx pp tp1)
    (T.Pretty.pp_type pp_ctx pp tp2)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let ctor_redefinition ~pos ~ppos name =
  (pos, Printf.sprintf "Constructor %s is defined more than once" name,
    [ (ppos, "Here is a previous definition") ])

let cannot_resolve_named_param ~pos x =
  (pos, Printf.sprintf "Cannot resolve a parameter of name %s" x, [])

let cannot_resolve_implicit ~pos name =
  (pos, Printf.sprintf "Cannot resolve an implicit parameter %s" name, [])

let cannot_resolve_method ~pos ~pp owner name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg =
    Printf.sprintf
      "Cannot resolve a method %s associated with %s" name
      (string_of_method_owner ~pp ~pp_ctx owner)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let cannot_resolve_method_constr ~pos ~pp name sch =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "Cannot resolve a method %s of type %s"
    name (T.Pretty.pp_scheme pp_ctx pp sch)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let type_already_provided ~pos ~npos name =
  (pos,
    Printf.sprintf "Type %s is provided more than once" name,
    [ npos, "Here is the last definition" ])

let named_param_already_provided ~pos ~npos ~pp (name : Name.t) =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "%s is provided more than once"
    (string_of_name ~pp ~pp_ctx ~cap:true name)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx,
    [ npos, "Here is the last definition" ])

let method_already_provided ~pos ~npos name =
  (pos, Printf.sprintf "Method %s is provided more than once" name,
    [ npos, "Here is the last definition" ])

let named_param_provided_as_optional ~pos name =
  (pos, Printf.sprintf "Named parameter %s is provided as optional" name, [])

let ambiguous_method_inst ~pos name =
  (pos, 
    Printf.sprintf 
      ("There are more than one method named %s expected by this function. " ^^
       "They cannot be provided explicitly.") name, [])

let module_inst_after_method_inst ~pos =
  (pos, "Cannot instantiate with a module after explicit method instantiation", [])

let unknown_named_type_pattern ~pos name =
  (pos, Printf.sprintf "Type %s was not expected here" name, [])

let unknown_named_pattern ~pos ~pp name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = Printf.sprintf
    "A pattern for %s was not expected here"
    (string_of_name ~pp ~pp_ctx name)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let multiple_named_patterns ~pos ~ppos ~pp name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg =
    Printf.sprintf "Multiple patterns for %s"
      (string_of_name ~pp ~pp_ctx name)
  in
  (pos, msg ^ T.Pretty.additional_info pp_ctx,
    [ ppos, "Here is a previous pattern" ])

let method_pattern_not_allowed ~pos =
  (pos, "Method patterns are not allowed", [])

let open_pattern_not_allowed ~pos =
  (pos, "Opening patterns are not allowed in the inference mode", [])

let duplicate_type_in_pattern ~pos ~ppos name =
  (pos,
    Printf.sprintf "Type %s is bound more than once in the same pattern" name,
    [ ppos, "Here is the previous binding" ])

let duplicate_val_in_pattern ~pos ~ppos ~pp name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg =
    Printf.sprintf "%s is bound more than once in the same pattern"
      (string_of_val_name ~pp ~pp_ctx ~cap:true name)
  in
  (pos, msg ^ T.Pretty.additional_info pp_ctx,
    [ ppos, "Here is the previous binding" ])

let duplicate_module_in_pattern ~pos ~ppos name =
  (pos,
    Printf.sprintf "Module %s is bound more than once in the same pattern"
      name,
    [ ppos, "Here is the previous binding" ])

let multiple_named_type_args ~pos ~ppos (name : S.tvar) =
  (pos,
    Printf.sprintf "Named type %s is bound more than once in single definition"
      name,
    [ ppos, "Here is the previous type binder with this name" ])

let multiple_named_args ~pos ~ppos ~pp (name : Name.t) =
  let pp_ctx = T.Pretty.empty_context () in
  let msg =
    Printf.sprintf "%s is bound more than once"
      (string_of_name ~pp ~pp_ctx ~cap:true name) in
  (pos, msg ^ T.Pretty.additional_info pp_ctx,
    [ ppos, "Here is the previous parameter with this name" ])

let generalized_type_clash ~pos name =
  (pos,
    Printf.sprintf
      "Implicitly generalized type %s clashes with other named type parameters"
        name, [])

let generalized_name_clash ~pos ~pp name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg = 
    Printf.sprintf
      "Implicitly generalized %s clashes with other named parameters"
      (string_of_name ~pp ~pp_ctx name)
  in (pos, msg ^ T.Pretty.additional_info pp_ctx, [])

let ctor_type_arg_same_as_data_arg ~pos (name : S.tvar) =
  (pos,
    Printf.sprintf "Named type %s is already bound by datatype itself"
      name, [])

let ctor_arity_mismatch ~pos cpath req_n prov_n =
  (pos,
    Printf.sprintf
      "Constructor %s expects %d parameter(s), but is applied to %d"
      (string_of_path cpath) req_n prov_n,
    [])

let redundant_named_type ~pos name =
  (pos, Printf.sprintf
      "Providing type %s to a function that do not expect it" name, [])

let redundant_named_parameter ~pos ~pp name =
  let pp_ctx = T.Pretty.empty_context () in
  let msg =
    Printf.sprintf "Providing %s to a function that do not expect it"
      (string_of_name ~pp ~pp_ctx name) in
  (pos, msg, [])

let redundant_method_parameter ~pos name =
  (pos, Printf.sprintf
      "Providing method %s to a function that do not expect it" name, [])
