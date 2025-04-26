(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Propositional variables *)

type t =
  { uid   : UID.t;
    value : value option BRef.t
  }

and value =
  | True
  | False
  | SameAs of t

let fresh () =
  { uid   = UID.fresh ();
    value = BRef.create None
  }

let rec value x =
  match BRef.get x.value with
  | None -> SameAs x
  | Some ((True | False) as v) -> v
  | Some (SameAs y) ->
    let v = value y in
    BRef.set x.value (Some v);
    v

let set_bool x b =
  match BRef.get x.value with
  | None ->
    BRef.set x.value (Some (if b then True else False))
  | Some _ ->
    assert false

let to_sexpr x = SExpr.Sym (UID.to_string x.uid)

module Ordered = struct
  type nonrec t = t
  let compare x y = UID.compare x.uid y.uid
end

module Set = Set.Make(Ordered)
module Map = Map.Make(Ordered)
