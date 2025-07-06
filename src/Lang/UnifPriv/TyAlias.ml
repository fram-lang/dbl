(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type aliases *)

module Ordered = struct
  type t =
    { uid    : UID.t;
      pp_uid : PPTree.uid;
      scope  : Scope.t
    }

  let compare x y = UID.compare x.uid y.uid
end
include Ordered

let fresh ?pp_uid ~scope () =
  assert (not (Scope.equal scope Scope.root));
  let uid = UID.fresh () in
  { uid    = uid;
    pp_uid =
      begin match pp_uid with
      | Some pp_uid -> pp_uid
      | None -> PP_UID uid
      end;
    scope  = scope
  }

let equal a b = a == b

let pp_uid a = a.pp_uid

let in_scope a scope = Scope.mem a.scope scope

module Map = Map.Make(Ordered)
