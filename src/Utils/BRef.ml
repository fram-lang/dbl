(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Backtrackable references *)

type world_state =
  | Active    (** State of new world -- can change to Committed or Invalid *)
  | Committed (** Committed world -- cannot change *)
  | Invalid   (** Invalid world -- cannot change, and values must backtrack *)

(** Worlds form a tree (with pointers towards the root) where nodes contain
  mutable state of the world. *)
type world =
  | WRoot
  | WNode of { mutable state : world_state; parent : world }

type 'a parent =
  | PRoot
  | PSaved of
    { value  : 'a;
      world  : world;
      parent : 'a parent
    }

type 'a t =
  { mutable value  : 'a;
    mutable world  : world;
    mutable parent : 'a parent
  }

(** Current world *)
let top_world = ref WRoot

let create v =
  { value  = v
  ; world  = !top_world
  ; parent = PRoot
  }

let equal r1 r2 = r1 == r2

(** Backtrack if world of given reference is invalid *)
let rec fix_world r =
  match r.world with
  | WRoot       -> ()
  | WNode wnode ->
    begin match wnode.state with
    | Active    -> ()
    | Committed ->
      (* The world is committed, should propagate changes *)
      propagate_commit wnode.parent r
    | Invalid   ->
      begin match r.parent with
      | PRoot -> failwith "BRef: Use of value created in an invalid world"
      | PSaved p ->
        (* Backtrack and try again *)
        r.value  <- p.value;
        r.world  <- p.world;
        r.parent <- p.parent;
        fix_world r
      end
    end

and propagate_commit world r =
  r.world <- world;
  match r.parent with
  | PSaved p when world == p.world ->
    (* Saved value from parent world can be discarded, because the world was
      committed. *)
    r.parent <- p.parent;
    fix_world r
  | PRoot | PSaved _ ->
    fix_world r

let get r =
  fix_world r;
  r.value

let rec set r v =
  fix_world r;
  if r.world == !top_world then
    r.value <- v
  else begin
    (* Save old value for potential backtracking *)
    r.parent <- PSaved
      { value  = r.value;
        world  = r.world;
        parent = r.parent
      };
    (* Set new value and world *)
    r.value <- v;
    r.world <- !top_world
  end

let bracket f =
  top_world := WNode { state = Active; parent = !top_world };
  match f () with
  | v ->
    begin match !top_world with
    | WRoot   -> assert false (* Stack discipline was violated. *)
    | WNode w ->
      w.state <- Committed;
      top_world := w.parent;
      v
    end
  | exception ex ->
    begin match !top_world with
    | WRoot   -> assert false (* Stack discipline was violated. *)
    | WNode w ->
      w.state <- Invalid;
      top_world := w.parent;
      raise ex
    end
