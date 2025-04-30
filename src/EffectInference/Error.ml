(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to effect-inference *)

open Common

type t = Position.t * string

let fatal (pos, msg) =
  InterpLib.Error.report ~pos ~cls:FatalError msg;
  raise InterpLib.Error.Fatal_error

let warn (pos, msg) =
  InterpLib.Error.report ~pos ~cls:Warning msg

(* ========================================================================= *)

let rec report_from_origin pp_ctx origin =
  match origin with
  | OExprType(pos, pp, tp1, tp2) ->
    let msg =
      Printf.sprintf
        ("This expression has type %s, " ^^
         "but an expression was expected of type %s")
        (T.Pretty.pp_type pp_ctx pp tp1)
        (T.Pretty.pp_type pp_ctx pp tp2)
    in (pos, pp, msg)

  | OExprEffect(pos, pp, eff1, eff2) ->
    let msg =
      Printf.sprintf
        ("This expression has effect %s, " ^^
         "but an expression was expected of effect %s")
        (T.Pretty.pp_effect pp_ctx pp eff1)
        (T.Pretty.pp_effect pp_ctx pp eff2)
    in (pos, pp, msg)

  | OInst(pos, pp, _, _, _) ->
    (pos, pp, "Cannot satisfy constraints required by this function.")

  | ODataScope(pos, pp, tp, Pure) ->
    let msg =
      Printf.sprintf
        "Ill-formed local definition. This expression has type %s"
        (T.Pretty.pp_type pp_ctx pp tp)
    in (pos, pp, msg)

  | ODataScope(pos, pp, tp, Impure eff) ->
    let msg =
      Printf.sprintf
        ("Ill-formed local definition. This expression has type %s " ^^
         "and effect %s")
        (T.Pretty.pp_type   pp_ctx pp tp)
        (T.Pretty.pp_effect pp_ctx pp eff)
    in (pos, pp, msg)

  | OHandlerScope(pos, pp, eff_var, tp) ->
    let msg =
      Printf.sprintf
        "Ill-formed handler of effect %s. This expression has type %s"
        (T.Pretty.pp_tvar pp_ctx pp eff_var)
        (T.Pretty.pp_type pp_ctx pp tp)
    in (pos, pp, msg)

  | OPatternType(pos, pp, tp2, tp1) ->
    let msg =
      Printf.sprintf
        ("This pattern matches values of type %s,"
        ^^ " but it was expected to match values of type %s")
        (T.Pretty.pp_type pp_ctx pp tp1)
        (T.Pretty.pp_type pp_ctx pp tp2)
    in (pos, pp, msg)

  | OPatternScheme(pos, pp, sch2, sch1) ->
    let msg =
      Printf.sprintf
        ("This pattern matches values of type %s,"
        ^^ " but it was expected to match values of type %s")
        (T.Pretty.pp_scheme pp_ctx pp sch1)
        (T.Pretty.pp_scheme pp_ctx pp sch2)
    in (pos, pp, msg)

  | OLabelAnnot(pos, pp, tp1, tp2) ->
    let msg =
      Printf.sprintf
        "This label has type %s, but is annotated with type %s"
        (T.Pretty.pp_type pp_ctx pp tp1)
        (T.Pretty.pp_type pp_ctx pp tp2)
    in (pos, pp, msg)

  | OSubEffect(origin, eff1, eff2) ->
    let (pos, pp, msg1) = report_from_origin pp_ctx origin in
    let msg2 =
      Printf.sprintf
        "\n  Cannot ensure that %s is a subeffect of %s"
        (T.Pretty.pp_effect pp_ctx pp eff1)
        (T.Pretty.pp_effect pp_ctx pp eff2)
    in (pos, pp, msg1 ^ msg2)

  | OEffectEquiv(origin, eff1, eff2) ->
    let (pos, pp, msg1) = report_from_origin pp_ctx origin in
    let msg2 =
      Printf.sprintf
        "\n  Cannot ensure that %s equal to %s"
        (T.Pretty.pp_effect pp_ctx pp eff1)
        (T.Pretty.pp_effect pp_ctx pp eff2)
    in (pos, pp, msg1 ^ msg2)

(* ========================================================================= *)

let escaping_effect_var ~origin x =
  let pp_ctx = T.Pretty.empty_context () in
  let (pos, pp, msg1) = report_from_origin pp_ctx origin in
  let msg2 =
    Printf.sprintf "\n  Effect variable %s escapes its scope."
      (T.Pretty.pp_tvar pp_ctx pp x)
  in
  (pos, msg1 ^ msg2 ^ T.Pretty.additional_info pp_ctx)

let non_exhaustive_match ~pos ctx =
  (* TODO: counterexample in the message. *)
  let msg = Printf.sprintf
    "This pattern-matching is not exhaustive."
  in (pos, msg)

let unused_pattern ~pos =
  (pos, "This pattern is unused")

let unsolved_unification_variable ~pos =
  (pos, "Unsolved unification variable left")
