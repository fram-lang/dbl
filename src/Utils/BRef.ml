(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Backtrackable references *)

type world_state =
| Active   (** State of new world -- can change to Commited or Invalid *)
| Commited (** Commited world -- cannot change *)
| Invalid  (** Invalid world -- cannot change, and values must backtrack *)

type world = world_state ref

type 'a parent =
| PRoot
| PSaved of
  { value  : 'a
  ; world  : world
  ; parent : 'a parent
  }

type 'a t =
  { mutable value  : 'a
  ; mutable world  : world
  ; mutable parent : 'a parent
  }

let top_world = ref (ref Active)

let create v =
  { value  = v
  ; world  = !top_world
  ; parent = PRoot
  }

let equal r1 r2 = r1 == r2

(** Backtrack if world of given reference is invalid *)
let rec fix_world r =
  match !(r.world) with
  | Active   -> ()
  | Commited ->
    (* The world is commited, should propagate changes *)
    begin match r.parent with
    | PRoot -> () (* No other worlds to inform *)
    | PSaved p ->
      (* Collapse parent worlds *)
      r.world  <- p.world;
      r.parent <- p.parent;
      fix_world r
    end
  | Invalid ->
    begin match r.parent with
    | PRoot -> failwith "BRef: Use of value created in an invalid world"
    | PSaved p  ->
      (* Backtrack and try again *)
      r.value  <- p.value;
      r.world  <- p.world;
      r.parent <- p.parent;
      fix_world r
    end

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
      { value  = r.value
      ; world  = r.world
      ; parent = r.parent
      };
    (* Set new value and world *)
    r.value <- v;
    r.world <- !top_world
  end

let bracket f =
  let old_world = !top_world in
  let new_world = ref Active in
  top_world := new_world;
  match f () with
  | v ->
    new_world := Commited;
    top_world := old_world;
    v
  | exception ex ->
    new_world := Invalid;
    top_world := old_world;
    raise ex
