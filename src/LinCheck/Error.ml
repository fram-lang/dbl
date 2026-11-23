(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to linearity *)

type mode =
  | Affine

let mode_to_string_u = function
  | Affine -> "Affine"

type t = Position.t * string * (Position.t * string) list

let report_note (pos, msg) =
  InterpLib.Error.report ~pos ~cls:Note msg

let report (pos, msg, notes) =
  InterpLib.Error.report ~pos ~cls:Error msg;
  List.iter report_note notes

let opt_note use_pos msg =
  match use_pos with
  | [] -> []
  | pos :: _ -> [ (pos, msg) ]

let resource_used_twice ~def_at ~mode use_pos1 use_pos2 =
  let (pos, msg, notes) =
    match use_pos2 with
    | [] ->
      (def_at, Printf.sprintf "%s resource used more than once."
        (mode_to_string_u mode), [])
    | pos :: _ ->
      (pos, Printf.sprintf "%s resource used more than once."
        (mode_to_string_u mode), [ (def_at, "Resource defined here.") ])
  in
  let notes =
    notes @ opt_note use_pos1 "The previous use of the resource is here."
  in
  (pos, msg, notes)

let two_resources_used ~def1_at ~def2_at use_pos1 use_pos2 =
  match use_pos2 with
  | [] ->
    (def1_at, "Multiple resources used in the same context.",
      [ (def2_at, "The other resource is defined here.") ])
  | pos :: _ ->
    (pos, "Multiple resources used in the same context.",
      [ (def1_at, "The first resource is defined here.");
        (def2_at, "The second resource is defined here.") ]
      @ opt_note use_pos1 "The first resource is used here.")

let resource_leaks pos ~def_at ~mode use_pos =
  (pos,
    Printf.sprintf "%s resource leaks through the type of this expression."
      (mode_to_string_u mode),
    [ (def_at, "Resource defined here.") ]
    @ opt_note use_pos "The resource is used here.")

let invalid_resource_use pos ~def_at ~mode use_pos =
  (pos,
    Printf.sprintf "%s resource cannot be used in this context."
      (mode_to_string_u mode),
    [ (def_at, "Resource defined here.") ]
    @ opt_note use_pos "The resource is used here.")
