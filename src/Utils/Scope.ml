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

let enter parent =
  { uid    = UID.fresh ();
    level  = parent.level + 1;
    parent = Some parent
  }

let rec inter s1 s2 =
  if s1.level > s2.level then
    inter (Option.get s1.parent) s2
  else if s1.level < s2.level then
    inter s1 (Option.get s2.parent)
  else if s1.uid = s2.uid then
    s1
  else
    match s1.parent, s2.parent with
    | Some p1, Some p2 -> inter p1 p2
    | _ -> assert false

let rec mem s1 s2 =
  if s1.level < s2.level then
    mem s1 (Option.get s2.parent)
  else
    s1.uid = s2.uid
