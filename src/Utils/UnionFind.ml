
module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module Make(Ord: OrderedType) : sig
    type elt
    type set
    type t
    val empty : t
    val add : elt -> t -> t
    val union : elt -> elt -> t -> t
    val find : elt -> t -> set
    val same_set : elt -> elt -> t -> bool
end with type elt = Ord.t = struct
    module Set = Set.Make(Ord)

    type set = Set.t
    type elt = Ord.t
    type t = set list
    let empty: set list = []

    let add x self = (Set.singleton x) :: self

    let find x self =
        List.find (fun s -> Set.mem x s) self

    let same_set x y self =
        Ord.compare (Set.choose (find x self)) (Set.choose (find y self)) = 0

    let union x y self =
        let s1 = find x self in
        let s2 = find y self in
        let s = Set.union s1 s2 in
        let ls = List.filter
            (fun s' -> let c = Set.choose s' in
               c != Set.choose s1 && c != Set.choose s2)
            self
        in s :: ls

end

module IntUF = Make(Int)

let graph = IntUF.empty

let _ =
    graph
    |> IntUF.add 42
    |> IntUF.add 13
    |> IntUF.add 37
    |> IntUF.add 8
    |> IntUF.add 12
    |> IntUF.add 15
    |> IntUF.union 42 13
    |> IntUF.union 13 37
    |> IntUF.union 8 12
    |> IntUF.union 12 15
    (* |> IntUF.union 12 37 *)
    |> IntUF.same_set 42 8
