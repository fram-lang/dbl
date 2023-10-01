(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Heterogeneous maps with keys with one type parameter. *)

(* Author: Piotr Polesiuk, 2023 *)

(** Ordered type with single type parameter *)
module type OrderedType1 = sig
  type 'a t

  (** Get unique identifier of given key *)
  val uid : 'a t -> UID.t

  (** Heterogeneous equality of keys. *)
  val equal : 'a t -> 'b t -> ('a, 'b) Eq.t
end

(** Type with one parameter *)
module type Type1 = sig
  type 'a t
end

(** Signature of a map, where type parameters of key and value are
  correlated. *)
module type S1 = sig
  type 'a key
  type 'a value

  type t

  (** Empty map *)
  val empty : t

  (** Singleton map *)
  val singleton : 'a key -> 'a value -> t

  (** Extend map with a key-value pair, possibly overriding previous binding *)
  val add : 'a key -> 'a value -> t -> t

  (** Check if given map is empty *)
  val is_empty : t -> bool

  (** Lookup for value assigned to given key. Raises [Not_found] if there is
    no such a binding *)
  val find : 'a key -> t -> 'a value

  (** Lookup for value assigned to given key. Returns [None] if there is no
    such a binding *)
  val find_opt : 'a key -> t -> 'a value option
end

(** Signature of a map *)
module type S = sig
  type 'a key

  type 'v t

  (** Empty map *)
  val empty : 'v t

  (** Singleton map *)
  val singleton : 'a key -> 'v -> 'v t

  (** Extend map with a key-value pair, possibly overriding previous binding *)
  val add : 'a key -> 'v -> 'v t -> 'v t

  (** Check if given map is empty *)
  val is_empty : 'v t -> bool

  (** Lookup for value assigned to given key. Raises [Not_found] if there is
    no such a binding *)
  val find : 'a key -> 'v t -> 'v

  (** Lookup for value assigned to given key. Returns [None] if there is no
    such a binding *)
  val find_opt : 'a key -> 'v t -> 'v option

  (** Make a finite map, where type parameters of key and value are
    correlated. *)
  module Make(Val : Type1) : S1
    with type 'a key   = 'a key
    and  type 'a value = 'a Val.t
end

(** Finite maps *)
module Make(Key : OrderedType1) :
  S with type 'a key = 'a Key.t =
struct
  type 'a key = 'a Key.t

  type 'v t = 'v UID.Map.t

  let empty = UID.Map.empty

  let singleton k v = UID.Map.singleton (Key.uid k) v

  let add k v m =
    UID.Map.add (Key.uid k) v m

  let is_empty = UID.Map.is_empty

  let find k m =
    UID.Map.find (Key.uid k) m

  let find_opt k m =
    UID.Map.find_opt (Key.uid k) m

  module Make(Val : Type1) = struct
    type nonrec 'a key = 'a key
    type 'a value = 'a Val.t

    type key_value = KV : 'a key * 'a value -> key_value

    type nonrec t = key_value t

    let empty = empty

    let singleton k v = singleton k (KV(k, v))

    let add k v m =
      add k (KV(k, v)) m

    let is_empty = is_empty

    let find (type a) (k : a key) m : a value =
      let (KV(k', v)) = find k m in
      match Key.equal k k' with
      | Equal    -> v
      | NotEqual -> assert false

    let find_opt (type a) (k : a key) m : a value option =
      match find_opt k m with
      | None -> None
      | Some (KV(k', v)) ->
        begin match Key.equal k k' with
        | Equal    -> Some v
        | NotEqual -> assert false
        end
  end
end
