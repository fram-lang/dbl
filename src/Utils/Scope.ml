(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scope of type variables. *)

type t =
  { uid    : UID.t;
    level  : int;
    parent : t option
  }

let root =
  { uid    = UID.fresh ();
    level  = 0;
    parent = None
  }

let any =
  { uid    = UID.fresh ();
    level  = -1;
    parent = None
  }

let enter parent =
  assert (parent.level >= 0);
  { uid    = UID.fresh ();
    level  = parent.level + 1;
    parent = Some parent
  }

let initial = enter root

let parent s = Option.get s.parent

let rec inter s1 s2 =
  if s1.level > s2.level then
    inter (parent s1) s2
  else if s1.level < s2.level then
    inter s1 (parent s2)
  else if s1.uid = s2.uid then
    s1
  else
    match s1.parent, s2.parent with
    | Some p1, Some p2 -> inter p1 p2
    | _ -> assert false

let equal s1 s2 =
  s1.uid = s2.uid

let subset s1 s2 =
  (* Edge case: [any] scope *)
  if s1.level < 0 || s2.level < 0 then
    s1.level = s2.level
  else
    let rec loop s1 s2 =
      if s1.level < s2.level then
        loop s1 (parent s2)
      else
        s1.uid = s2.uid
    in
    loop s1 s2

let strict_subset s1 s2 =
  if s2.level <= 0 then
    false
  else
    subset s1 (parent s2)

let mem = subset
