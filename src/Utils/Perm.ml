(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Partial finite permutations *)

(** Signature of a module with partial finite permutations *)
module type S = sig
  (** Type of the permutation keys *)
  type key

  (** Module of a sets of keys *)
  module KeySet : Set.S with type elt = key

  (** Type of permutations from [key] to [key] *)
  type t

  (** Identity permutation *)
  val id : t

  (** Inverse permutation *)
  val inverse : t -> t

  (** Permutation that swaps two keys *)
  val swap : key -> key -> t

  (** Composition of two permutation. The order of parameters is the same as
    for function composition, i.e., [apply (compose p1 p2) x] is equivalent to
    [apply p1 (apply p2 x)]. *)
  val compose : t -> t -> t

  (** Extend the permutation with swapping with a fresh key, before applying the
    rest of the permutation. [swap_with_fresh_r p x y] is equivalent to
    [compose p (swap x y)], assuming that [y] is fresh: it not appear in the
    domain nor codomain of the partial permutation. *)
  val swap_with_fresh_r : t -> key -> key -> t

  (** Leave only those mappings, that are in given domain *)
  val shrink_dom : KeySet.t -> t -> t

  (** Map elements of given set *)
  val map_set : t -> KeySet.t -> KeySet.t

  (** Check if a permutation is a (partial) identity *)
  val is_identity : t -> bool

  (** Check if two permutations agree on given key *)
  val agree_on : t -> t -> key -> bool

  (** Apply a permutation to given key. The result on elements removed from
    a domain is not specified *)
  val apply : t -> key -> key
end

(** A functor that creates an implementation of permutations for given keys *)
module Make(Key : Set.OrderedType)(KeySet : Set.S with type elt = Key.t) :
  S with type key = Key.t and module KeySet = KeySet =
struct
  type   key    = Key.t
  module KeySet = KeySet

  module KeyMap = Map.Make(Key)

  type t = {
    fwd : key KeyMap.t;
      (** Forward function. Keys that are mapped to themself are not included
        in this map. *)

    bck : key KeyMap.t
      (** Backward function. Keys that are mapped to themself are not included
        in this map. *)
  }

  let id = { fwd = KeyMap.empty; bck = KeyMap.empty }

  let inverse p = { fwd = p.bck; bck = p.fwd }

  let swap x y =
    if Key.compare x y = 0 then id
    else
      let m = KeyMap.add x y (KeyMap.add y x KeyMap.empty) in
      { fwd = m; bck = m }

  let compose_map m2 m1 =
    let merge_f key v2 v1 =
      match v1 with
      | None   -> v2
      | Some v ->
        begin match KeyMap.find_opt v m2 with
        | None -> v1
        | Some x when Key.compare key x = 0 -> None
        | r -> r
        end
    in
    KeyMap.merge merge_f m2 m1

  let compose p2 p1 =
    { fwd = compose_map p2.fwd p1.fwd;
      bck = compose_map p1.bck p2.bck
    }

  let swap_with_fresh_r p x y =
    assert (Key.compare x y <> 0);
    { fwd = KeyMap.add x y p.fwd;
      bck = KeyMap.add y x p.bck
    }

  let shrink_dom dom p =
    { fwd = KeyMap.filter (fun k _ -> KeySet.mem k dom) p.fwd;
      bck = KeyMap.filter (fun _ v -> KeySet.mem v dom) p.bck
    }

  let apply p x =
    match KeyMap.find_opt x p.fwd with
    | None   -> x
    | Some x -> x

  let map_set p s =
    KeySet.map (apply p) s

  let is_identity p = KeyMap.is_empty p.fwd

  let agree_on p1 p2 x =
    Key.compare (apply p1 x) (apply p2 x) = 0
end
