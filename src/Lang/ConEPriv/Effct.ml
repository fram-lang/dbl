(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of effects in the ConE language. *)

(* The effect is represented as a pair of maps: one from regular effect
  variables, and one from generalizable variables. Both maps assigns formula
  to a variable, indicating if the variable is present in the effect. The
  second map is represented as a map from UIDs, because of the circular
  dependencies between types. *)

type formula = IncrSAT.Formula.t

type t =
  { tvars : formula TVar.Map.t BRef.t;
    gvars : gvar_map BRef.t
  }

and gvar_map = (gvar * formula) UID.Map.t

and gvar =
  { uid   : UID.t;
    state : gvar_state BRef.t
  }

and gvar_state =
  | GVar   of Scope.t
    (** Proper generalizable variable that belongs to the given scope. *)

  | Effect of t
    (** The generalizable variable was globally replaced with given effect. *)

(* ========================================================================= *)
module TVarMap : sig
  type t = formula TVar.Map.t

  val empty     : t
  val singleton : TVar.t -> formula -> t
  val filter    : (TVar.t -> formula -> bool) -> t -> t
  val add       : TVar.t -> formula -> t -> t
  val union     : t -> t -> t
  val guard     : t -> formula -> t
  val lookup    : t -> TVar.t -> formula
  val to_list   : t -> (TVar.t * formula) list
end = struct
  type t = formula TVar.Map.t

  let empty = TVar.Map.empty
  let singleton = TVar.Map.singleton
  let filter = TVar.Map.filter

  let add x p m =
    match TVar.Map.find_opt x m with
    | None    -> TVar.Map.add x p m
    | Some p2 -> TVar.Map.add x (IncrSAT.Formula.disj p p2) m

  let union =
    TVar.Map.union (fun _ p1 p2 -> Some (IncrSAT.Formula.disj p1 p2))

  let guard m p =
    if IncrSAT.Formula.is_true p then m
    else TVar.Map.map (IncrSAT.Formula.conj p) m

  let lookup m x =
    match TVar.Map.find_opt x m with
    | None   -> IncrSAT.Formula.bot
    | Some p -> p

  let to_list = TVar.Map.bindings
end

(* ========================================================================= *)
module GVarMap : sig
  type t = gvar_map

  val empty     : t
  val singleton : gvar -> formula -> t
  val filter    : (gvar -> formula -> bool) -> t -> t
  val add       : gvar -> formula -> t -> t
  val fold      : (gvar -> formula -> 'a -> 'a) -> t -> 'a -> 'a
  val union     : t -> t -> t
  val guard     : t -> formula -> t
  val lookup    : t -> gvar -> formula
  val to_list   : t -> (gvar * formula) list
end = struct
  type t = gvar_map

  let empty = UID.Map.empty
  let singleton gv p = UID.Map.singleton gv.uid (gv, p)
  let filter f m = UID.Map.filter (fun _ (gv, p) -> f gv p) m

  let add gv p m =
    match UID.Map.find_opt gv.uid m with
    | None         -> UID.Map.add gv.uid (gv, p) m
    | Some (_, p2) -> UID.Map.add gv.uid (gv, IncrSAT.Formula.disj p p2) m

  let fold f m a = UID.Map.fold (fun _ (gv, p) -> f gv p) m a

  let union =
    UID.Map.union
      (fun _ (u1, p1) (u2, p2) ->
        assert (u1.uid = u2.uid);
        Some (u1, IncrSAT.Formula.disj p1 p2))

  let guard m p =
    if IncrSAT.Formula.is_true p then m
    else UID.Map.map (fun (gv, p1) -> (gv, IncrSAT.Formula.conj p1 p)) m

  let lookup m gv =
    match UID.Map.find_opt gv.uid m with
    | None       -> IncrSAT.Formula.bot
    | Some(_, p) -> p

  let to_list m = UID.Map.bindings m |> List.map snd
end

(* ========================================================================= *)

let pure =
  { tvars = BRef.create TVarMap.empty;
    gvars = BRef.create GVarMap.empty
  }

let var x =
  { tvars = BRef.create (TVarMap.singleton x IncrSAT.Formula.top);
    gvars = BRef.create GVarMap.empty
  }

let gvar gv =
  { tvars = BRef.create TVarMap.empty;
    gvars = BRef.create (GVarMap.singleton gv IncrSAT.Formula.top)
  }

let cons x p eff =
  { tvars = BRef.create (TVarMap.add x p (BRef.get eff.tvars));
    gvars = BRef.create (BRef.get eff.gvars)
  }

let join eff1 eff2 =
  { tvars =
      BRef.create (TVarMap.union (BRef.get eff1.tvars) (BRef.get eff2.tvars));
    gvars =
      BRef.create (GVarMap.union (BRef.get eff1.gvars) (BRef.get eff2.gvars))
  }

let guard eff p =
  if IncrSAT.Formula.is_true p then eff
  else
    { tvars = BRef.create (TVarMap.guard (BRef.get eff.tvars) p);
      gvars = BRef.create (GVarMap.guard (BRef.get eff.gvars) p)
    }

let fresh_gvar ~scope =
  gvar { uid = UID.fresh (); state = BRef.create (GVar scope) }

(* ========================================================================= *)

module GVar = struct
  module Ordered = struct
    type t = gvar
    let compare gv1 gv2 = UID.compare gv1.uid gv2.uid
  end

  let scope gv =
    match BRef.get gv.state with
    | GVar scope -> scope
    | Effect _ -> assert false

  let get_scope = scope

  let in_scope gv scope =
    Scope.mem (get_scope gv) scope

  let uid gv = gv.uid

  let update_scope ~scope ~tvars gv =
    let tvars = List.filter (Fun.flip TVar.in_scope (get_scope gv)) tvars in
    match tvars, BRef.get gv.state with
    | [], GVar old_scope ->
      assert (Scope.subset scope old_scope);
      BRef.set gv.state (GVar scope);
      gv

    | _, GVar old_scope ->
      let new_gv =
        { uid   = UID.fresh ();
          state = BRef.create (GVar scope)
        } in
      let tm =
        List.fold_left
          (fun tm x -> TVarMap.add x (IncrSAT.Formula.fresh_var ()) tm)
          TVarMap.empty
          tvars in
      let eff =
        { tvars = BRef.create tm;
          gvars = BRef.create (GVarMap.singleton new_gv IncrSAT.Formula.top)
        } in
      BRef.set gv.state (Effect eff);
      new_gv

    | _, Effect _ -> assert false

  let set gv eff =
    match BRef.get gv.state with
    | GVar   _ -> BRef.set gv.state (Effect eff)
    | Effect _ -> assert false

  let fix gv =
    let x = TVar.fresh_eff ~scope:(scope gv) in
    set gv (var x);
    x

  module Set = Set.Make(Ordered)
  module Map = Map.Make(Ordered)

  let to_sexpr gv =
    match BRef.get gv.state with
    | GVar scope ->
      SExpr.List [ Sym ("?" ^ UID.to_string gv.uid); Scope.to_sexpr scope ]
    | Effect _ -> SExpr.Sym ("effect")
end

(* ========================================================================= *)

let rec view_map eff =
  let tm =
    BRef.get eff.tvars
    |> TVarMap.filter (fun _ p -> not (IncrSAT.Formula.is_false p)) in
  let (tm, gm) =
    GVarMap.fold view_add_gvar (BRef.get eff.gvars) (tm, GVarMap.empty) in
  BRef.set eff.tvars tm;
  BRef.set eff.gvars gm;
  (tm, gm)

and view_add_gvar gv p (tm, gm) =
  if IncrSAT.Formula.is_false p then (tm, gm)
  else
    match BRef.get gv.state with
    | GVar _     -> (tm, GVarMap.add gv p gm)
    | Effect eff ->
      let (tm2, gm2) = view_map eff in
      let tm = TVarMap.union tm (TVarMap.guard tm2 p) in
      let gm = GVarMap.union gm (GVarMap.guard gm2 p) in
      (tm, gm)

let view eff =
  let (tm, gm) = view_map eff in
  (TVarMap.to_list tm, GVarMap.to_list gm)

let take_tvars eff =
  let (tm, gm) = view_map eff in
  let eff =
    { tvars = BRef.create TVarMap.empty;
      gvars = BRef.create gm
    } in
  (TVarMap.to_list tm, eff)

let lookup_tvar eff x =
  let (tm, _) = view_map eff in
  TVarMap.lookup tm x

let lookup_gvar eff gv =
  let (_, gm) = view_map eff in
  GVarMap.lookup gm gv

(* ========================================================================= *)

let filter_to_scope ~scope eff =
  let (tm, gm) = view_map eff in
  { tvars =
      TVarMap.filter (fun x  _ -> TVar.in_scope x  scope) tm
      |> BRef.create;
    gvars =
      GVarMap.filter (fun gv _ -> GVar.in_scope gv scope) gm
      |> BRef.create
  }

(* ========================================================================= *)

let collect_gvars ~scope eff gvs =
  let (_, gm) = view_map eff in
  GVarMap.fold
    (fun gv _ gvs ->
      if GVar.in_scope gv scope then gvs
      else GVar.Set.add gv gvs)
    gm
    gvs

(* ========================================================================= *)

let guarded_to_sexpr to_sexpr (x, p) =
  if IncrSAT.Formula.is_true p then to_sexpr x
  else SExpr.List [ to_sexpr x; Sym "?"; IncrSAT.Formula.to_sexpr p ]

let to_sexpr eff =
  let (tvs, gvs) = view eff in
  SExpr.List
    (List.map (guarded_to_sexpr TVar.to_sexpr) tvs @
     List.map (guarded_to_sexpr GVar.to_sexpr) gvs)
