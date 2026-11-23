(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Types used by the linearity checking. *)

type mode = Error.mode =
  | Affine

type resource_id =
  { r_uid  : UID.t;
    r_pos  : Position.t;
    r_mode : mode
  }

type use_pos = Position.t list

type usage =
  | RNone
  | RSome of resource_id * use_pos

type typ =
  | TAny
  | TArrow of typ * usage

let fresh ~pos mode =
  { r_uid  = UID.fresh ();
    r_pos  = pos;
    r_mode = mode
  }

let arrow tp rs =
  match tp, rs with
  | TAny, RNone -> TAny
  | _ -> TArrow(tp, rs)

let as_arrow ~pos tp =
  match tp with
  | TAny              -> (TAny, RNone)
  | TArrow(tp, RNone) -> (tp, RNone)
  | TArrow(tp, RSome(rsid, use_pos)) ->
    (tp, RSome(rsid, pos :: use_pos))

let is_none rs =
  match rs with
  | RNone   -> true
  | RSome _ -> false

let seq_join rs1 rs2 =
  match rs1, rs2 with
  | RNone, rs | rs, RNone -> rs
  | RSome(rsid1, use_pos1), RSome(rsid2, use_pos2)
      when UID.equal rsid1.r_uid rsid2.r_uid ->
    Error.report
      (Error.resource_used_twice ~mode:rsid1.r_mode ~def_at:rsid1.r_pos
        use_pos1 use_pos2);
    rs2
  | RSome(rsid1, use_pos1), RSome(rsid2, use_pos2) ->
    Error.report
      (Error.two_resources_used ~def1_at:rsid1.r_pos ~def2_at:rsid2.r_pos
        use_pos1 use_pos2);
    rs2

let par_join rs1 rs2 =
  match rs1, rs2 with
  | RNone, RNone -> RNone
  | RSome(rsid1, use_pos), RSome(rsid2, _)
      when UID.equal rsid1.r_uid rsid2.r_uid ->
    RSome(rsid1, use_pos)

  | RSome(rsid1, use_pos1), RSome(rsid2, use_pos2) ->
    Error.report
      (Error.two_resources_used ~def1_at:rsid1.r_pos ~def2_at:rsid2.r_pos
        use_pos1 use_pos2);
    rs2

  | RSome({ r_mode = Affine; _}, _), RNone -> rs1
  | RNone, RSome({ r_mode = Affine; _}, _) -> rs2

let rec join_types tp1 tp2 =
  match tp1, tp2 with
  | TAny, TAny -> TAny
  | TAny, TArrow(tp, rs) | TArrow(tp, rs), TAny ->
    TArrow(tp, par_join rs RNone)
  | TArrow(tp1, rs1), TArrow(tp2, rs2) ->
    TArrow(join_types tp1 tp2, par_join rs1 rs2)

let rec ensure_no_resources_t e tp =
  match tp with
  | TAny -> ()
  | TArrow(tp, RNone) -> ensure_no_resources_t e tp
  | TArrow(tp, RSome(rsid, use_pos)) ->
    Error.report
      (Error.resource_leaks e.Lang.Unif.pos
        ~mode:rsid.r_mode ~def_at:rsid.r_pos use_pos);
    ensure_no_resources_t e tp

let ensure_no_resources_r e rs =
  match rs with
  | RNone -> ()
  | RSome(rsid, use_pos) ->
    Error.report
      (Error.invalid_resource_use e.Lang.Unif.pos
        ~mode:rsid.r_mode ~def_at:rsid.r_pos use_pos)

let check_usage e rsid rs =
  match rsid.r_mode, rs with
  | Affine, RNone -> ()
  | Affine, RSome(rsid2, _)
    when UID.equal rsid.r_uid rsid2.r_uid -> ()
  | Affine, RSome(rsid2, use_pos) ->
    Error.report
      (Error.invalid_resource_use e.Lang.Unif.pos
        ~mode:rsid.r_mode ~def_at:rsid.r_pos use_pos)

let guard e rs =
  let open Lang.Unif in
  match rs with
  | RNone -> e
  | RSome(rsid, use_pos) ->
    let mode =
      match rsid.r_mode with
      | Affine -> EffectMode.Affine
    in
    { e with data = EModeAnnot
      { body = e;
        mode;
        resource_pos = rsid.r_pos;
        resource_use = use_pos
      }
    }
