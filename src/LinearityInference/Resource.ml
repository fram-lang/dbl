(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Types used by the linearity constraint inference. *)

type mode =
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
  | RNone -> true

let seq_join rs1 rs2 =
  match rs1, rs2 with
  | RNone, rs | rs, RNone -> rs

let par_join rs1 rs2 =
  match rs1, rs2 with
  | RNone, RNone -> RNone

let join_types tp1 tp2 =
  match tp1, tp2 with
  | TAny, TAny -> TAny

let ensure_no_resources_t e tp =
  match tp with
  | TAny -> ()

let ensure_no_resources_r e rs =
  match rs with
  | RNone -> ()

let check_usage e rsid rs =
  match rsid.r_mode, rs with
  | Affine, RNone -> ()
  | Affine, RSome(rsid2, _)
    when UID.equal rsid.r_uid rsid2.r_uid -> ()

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
