(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

module Ordered = struct
  type t = {
    uid       : UID.t;
    method_ns : UID.t;
    pp_uid    : PPTree.uid;
    kind      : Kind.t;
    scope     : Scope.t
  }

  let compare x y = UID.compare x.uid y.uid
end
include Ordered

let kind x = x.kind

let fresh ?method_ns ?pp_uid ~scope kind =
  assert (not (Scope.equal scope Scope.root));
  let uid = UID.fresh () in
  { uid       = uid;
    method_ns = Option.value method_ns ~default:uid;
    pp_uid    = Option.value pp_uid ~default:(PPTree.PP_UID uid);
    kind      = kind;
    scope     = scope
  }

let clone ~scope x =
  { x with uid = UID.fresh (); scope = scope }

let equal x y = x == y

let uid x = x.uid

let method_ns x = x.method_ns

let pp_uid x = x.pp_uid

let scope x = x.scope

let in_scope x scope = Scope.mem x.scope scope

module Set = Set.Make(Ordered)
module Map = Map.Make(Ordered)

let to_sexpr x =
  SExpr.List [
    SExpr.Sym (UID.to_string x.uid);
    Scope.to_sexpr x.scope
  ]
