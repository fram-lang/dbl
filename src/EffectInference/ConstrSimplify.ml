(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint simplification before generalization *)

open Common

(** Internal state of the simplification procedure. *)
type state =
  {         scope    : Scope.t;
      (** The current outer scope. Generalizable variables that do not belong
        to this scope can be altered by the simplification process. *)

    mutable progress : bool;
      (** Indicates if some progress was made during the last iteration. *)

    mutable pgvs     : T.GVar.Set.t;
      (** Positive generalizable variables, or more precisely, generalizable
        variables that appear on non-negative positions of the generalized
        type. Simplification procedure cares about variables that do not
        belong to [scope], so other variables may not be in this set, even
        if they are positive. *)

    mutable ngvs     : T.GVar.Set.t;
      (** Negative generalizable variables, or more precisely, generalizable
        variables that appear on negative positions of the generalized
        type. The variable may be present in both [pgvs] and [ngvs]. See
        [pgvs] for more details. *)

    mutable lhs_gvars : T.GVar.Set.t;
      (** Generalizable variables that appear on the left-hand-side of any
        constraint. When some rule makes progress, it is not responsible to
        maintain this set, therefore it is recomputed at each iteration. *)

    mutable rhs_gvars : T.GVar.Set.t;
      (** Generalizable variables that appear on the right-hand-side of any
        constraint. When some rule makes progress, it is not responsible to
        maintain this set, therefore it is recomputed at each iteration. *)

    mutable irr      : Constr.t list
      (** Irrelevant constraints, i.e., that do not contain any generalizable
        variable outside of [scope]. Such constraints are stored here to be
        re-added at the end of the simplification process. *)
  }

(** Effect variable (regular or generalizable). Generalizable variables are
  represented as effects, because they may be replaced during the
  simplification process. *)
type eff_var =
  | TVar of T.tvar
  | GVar of T.effct

(** Internal representation of a (normalized) constraint. *)
type constr =
  { origin     : origin;
      (** Origin of the constraint for error reporting. *)

    eff_var    : eff_var;
      (** Effect variable on the left-hand-side of the constraint. *)

    pformula   : T.formula;
      (** Formula guarding the presence of the left-hand-side variable. *)

    nformula   : T.formula;
      (** Negated formula guarding the presence of the left-hand-side.
        The ConE language doesn't support negative guards. However, for
        negative guards on the left-hand-side of a constraint
        ([eff1 ? ~p <: eff2]), we can build equivalent constraints with
        positive guards: [eff1 <: eff2 * (eff1 ? p)]. *)

    rhs_effect : T.effct
      (** Right-hand-side effect of the constraint. After the normalization,
        the right-hand-side effect does not contain [eff_var]. However, this
        property may be violated by setting values to generalizable variables
        during the simplification process. *)
  }

let eff_var_to_sexpr =
  function
  | TVar x   -> SExpr.List [ Sym "tvar"; T.TVar.to_sexpr x ]
  | GVar eff -> SExpr.List [ Sym "gvar"; T.Effct.to_sexpr eff ]

let constr_to_sexpr c =
  SExpr.List [
    eff_var_to_sexpr c.eff_var;
    List [ Sym "if";     IncrSAT.Formula.to_sexpr c.pformula ];
    List [ Sym "unless"; IncrSAT.Formula.to_sexpr c.nformula ];
    Sym "<:";
    T.Effct.to_sexpr c.rhs_effect
  ]

(* ========================================================================= *)
(* Basic operations *)

let initial_state ~scope ~pgvs ~ngvs =
  { scope;
    progress = false;
    pgvs;
    ngvs;
    lhs_gvars = T.GVar.Set.empty;
    rhs_gvars = T.GVar.Set.empty;
    irr = []
  }

let add_irrelevant_constr st c =
  st.irr <- c :: st.irr

(** Get the generalizable variable from an effect assumed to be a
  single generalizable variable. This function cannot be used after
  some generalizable variables have been set until the next iteration
  that normalizes the constraints again. *)
let get_gvar eff =
  match T.Effct.view eff with
  | ([], [(gv, p)]) when IncrSAT.Formula.is_true p -> gv
  | _ ->
    InterpLib.InternalError.report
      ~reason:"Violation of constraint simplification invariant: \
        Extracting generalizable variable from left-hand-side effect \
        after some generalizable variables have been set."
      ()

let effect_of_eff_var = function
  | TVar x    -> T.Effct.var x
  | GVar eff1 -> eff1

(** Module for effect variables, to be used in maps. It cannot be used
  after some generalizable variables have been set until the next
  iteration that normalizes the constraints again. *)
module EffVar = struct
  type t = eff_var
  let compare ev1 ev2 =
    match ev1, ev2 with
    | TVar x1,   TVar x2   -> T.TVar.compare x1 x2
    | TVar _,    _         -> -1
    | _,         TVar _    -> 1
    | GVar eff1, GVar eff2 -> T.GVar.compare (get_gvar eff1) (get_gvar eff2)
end

module EffVarMap = Map.Make(EffVar)

(* ========================================================================= *)
(* Constraint normalization *)

(** Check if effect [eff] is irrelevant, i.e., it does not contain any
  generalizable variable outside of the current outer scope. *)
let is_eff_irrelevant st eff =
  let (_, gvars) = T.Effct.view eff in
  List.for_all
    (fun (gv, _) -> T.GVar.in_scope gv st.scope)
    gvars

(** Check if generalizable variable [gv] is irrelevant, i.e., it belongs
  to the current outer scope. *)
let is_gvar_irrelevant st gv =
  T.GVar.in_scope gv st.scope

(** Normalize a constraint of the form [x ? p <: eff2], where [x] is a type
  variable. *)
let normalize_tvar_constr st orig eff2 (x, p) =
  let nformula = T.Effct.lookup_tvar eff2 x in
  if IncrSAT.Formula.implies p nformula then
    (* The constraint is trivially satisfied. *)
    []
  else if is_eff_irrelevant st eff2 then begin
    (* The constraint is irrelevant. *)
    let c = Constr.CSubeffect(orig, T.Effct.guard (T.Effct.var x) p, eff2) in
    add_irrelevant_constr st c;
    []
  end else
    let c =
      { origin     = orig;
        eff_var    = TVar x;
        pformula   = p;
        nformula   = nformula;
        rhs_effect = T.Effct.remove_tvar x eff2
      }
    in [ c ]

(** Normalize a constraint of the form [gv ? p <: eff2], where [gv] is a
  generalizable variable. *)
let normalize_gvar_constr st orig eff2 (gv, p) =
  let nformula = T.Effct.lookup_gvar eff2 gv in
  if IncrSAT.Formula.implies p nformula then
    (* The constraint is trivially satisfied. *)
    []
  else if is_gvar_irrelevant st gv && is_eff_irrelevant st eff2 then begin
    (* The constraint is irrelevant. *)
    let c =
      Constr.CSubeffect(orig, T.Effct.guard (T.Effct.gvar gv) p, eff2) in
    add_irrelevant_constr st c;
    []
  end else
    let c =
      { origin     = orig;
        eff_var    = GVar (T.Effct.gvar gv);
        pformula   = p;
        nformula   = nformula;
        rhs_effect = T.Effct.remove_gvar gv eff2
      }
    in [ c ]

(** Normalize a constraint. Returns a list of normalized constraints
  in the internal representation. *)
let normalize st (c : Constr.t) =
  let (CSubeffect(orig, eff1, eff2)) = c in
  let (tvs, gvs) = T.Effct.view eff1 in
  List.concat_map (normalize_tvar_constr st orig eff2) tvs @
  List.concat_map (normalize_gvar_constr st orig eff2) gvs

(** The inverse of [normalize], converting internal representation
  back to standard constraints. *)
let to_constr (c : constr) =
  let eff1 = T.Effct.guard (effect_of_eff_var c.eff_var) c.pformula in
  let eff2 =
    if IncrSAT.Formula.is_true c.nformula then
      c.rhs_effect
    else
      T.Effct.join c.rhs_effect (T.Effct.guard eff1 c.nformula)
  in
  Constr.CSubeffect(c.origin, eff1, eff2)

(* ========================================================================= *)
(* Maintenance of generalizable variable sets *)

(** [set_gvar st gv eff] sets the variable [gv] to the effect [eff], and
  updates the positive/negative sets accordingly. It is assumed that [gv] is
  not in the outer scope ([st.scope]) and that [eff] does not contain [gv]
  itself. *)
let set_gvar st gv eff =
  assert (IncrSAT.Formula.is_false (T.Effct.lookup_gvar eff gv));
  assert (not (T.GVar.in_scope gv st.scope));
  let (_, eff_gvs) = T.Effct.view eff in
  let eff_gvs =
    eff_gvs
    |> List.filter_map (fun (gv, _) ->
        if T.GVar.in_scope gv st.scope then None else Some gv)
    |> T.GVar.Set.of_list
  in
  if T.GVar.Set.mem gv st.pgvs then begin
    st.pgvs <- T.GVar.Set.remove gv (T.GVar.Set.union st.pgvs eff_gvs)
  end;
  if T.GVar.Set.mem gv st.ngvs then begin
    st.ngvs <- T.GVar.Set.remove gv (T.GVar.Set.union st.ngvs eff_gvs)
  end;
  T.GVar.set gv eff;
  st.progress <- true

(** Variables that appear on the left-hand-side of any constraint. *)
let lhs_gvars st cs =
  let scope = st.scope in
  List.fold_left
    (fun gvs (c : constr) ->
      match c.eff_var with
      | GVar eff_var ->
        T.Effct.collect_gvars ~scope eff_var gvs
      | TVar _ -> gvs)
    T.GVar.Set.empty
    cs

(** Variables that appear on the right-hand-side of any constraint. *)
let rhs_gvars st cs =
  let scope = st.scope in
  List.fold_left
    (fun gvs (c : constr) ->
      T.Effct.collect_gvars ~scope c.rhs_effect gvs)
    T.GVar.Set.empty
    cs

(* ========================================================================= *)
(* Rule: close positive generalizable variables *)

(** Sets variables that occur only positively or on the left-hand-side of
  constraints to pure effect. *)

let rule_close_positive st cs =
  let positive =
    T.GVar.Set.diff
      (T.GVar.Set.union st.lhs_gvars st.pgvs)
      (T.GVar.Set.union st.rhs_gvars st.ngvs)
  in
  T.GVar.Set.iter (fun gv -> set_gvar st gv T.Effct.pure) positive;
  cs

(* ========================================================================= *)
(* Rule: move up negative generalizable variables *)

(** Sets variables that occur only negatively to their upper-bound, if they
  have exactly one. *)

(** Update a map of upper-bounds for generalizable variables that occur
  negatively, based on a single constraint. The map associates each
  generalizable variable to [Some eff] if it has exactly one upper-bound
  [eff], or to [None] otherwise. In particular, [None] is used when the
  variable has multiple upper-bounds, or has at least one upper-bound that
  might be trivially satisfied. Variables that have no upper-bound are not
  present in the map. *)
let build_single_upper_bounds st bnd (c : constr) =
  match c.eff_var with
  | TVar _   -> bnd (* Not a generalizable variable. *)
  | GVar eff ->
    let gv = get_gvar eff in
    if T.GVar.in_scope gv st.scope then
      (* Variable is irrelevant (it is in the outer scope), ignore it. *)
      bnd
    else if T.GVar.Set.mem gv st.pgvs then
      (* Variable occurs positively, ignore it. *)
      bnd
    else if not (IncrSAT.Formula.is_true c.pformula) then
      (* Upper-bound might be trivially satisfied. *)
      T.GVar.Map.add gv None bnd
    else if not (IncrSAT.Formula.is_false c.nformula) then
      (* Upper-bound might be trivially satisfied. *)
      T.GVar.Map.add gv None bnd
    else
      match T.GVar.Map.find_opt gv bnd with
      | Some None ->
        (* Already multiple upper-bounds. *)
        bnd
      | Some (Some _) ->
        (* Now multiple upper-bounds. *)
        T.GVar.Map.add gv None bnd
      | None ->
        (* First upper-bound found. *)
        T.GVar.Map.add gv (Some c.rhs_effect) bnd

let rule_move_up_negative st cs =
  let bounds =
    List.fold_left (build_single_upper_bounds st) T.GVar.Map.empty cs in
  let bounds =
    List.filter_map
      (fun (gv, bnd) -> Option.map (fun eff -> (gv, eff)) bnd)
      (T.GVar.Map.bindings bounds)
  in
  List.iter
    (fun (gv, eff) ->
      (* Skip constraints, where [eff] contains [gv]. For normalized
        constraints it is impossible, but such constraints may appear after
        setting some generalizable variables in this loop. For instance, when
        we have [a <: b] and [b <: a]. *)
      if IncrSAT.Formula.is_false (T.Effct.lookup_gvar eff gv) then
        set_gvar st gv eff)
    bounds;
  cs

(* ========================================================================= *)
(* Rule: move down variant generalizable variables that do not have
    upper-bounds *)

(** Sets variant variables (with no positive nor negative occurrence) that
  have no upper-bound to the join of all their lower-bounds. *)

(** Join [lb] with the lower-bound provided by constraint [c] for
  generalizable variable [gv]. The bound is guarded by two formulas:
  - [c.pformula]: presence of the left-hand-side variable -- because this
    is part of the constraint;
  - the formula describing the presence of [gv] in the right-hand-side
    effect -- if [gv] is not present, then the constraint does not provide
    any lower-bound for it.

  The negative formula [c.nformula] is ignored, so the constructed bound may
  be less precise than what could be achieved. However, this is sufficient
  for the purpose of this simplification rule, because of the following:
  - the variable [gv] is a variant variable (it does not occur in the
    generalized type);
  - the variable [gv] has no upper-bound, so setting it to a larger effect
    cannot violate any constraint. *)
let join_lower_bound gv lb (c : constr) =
  let p = T.Effct.lookup_gvar c.rhs_effect gv in
  if IncrSAT.Formula.is_false p then
    lb
  else
    T.Effct.join lb
      (T.Effct.guard (effect_of_eff_var c.eff_var)
        (IncrSAT.Formula.conj c.pformula p))

let set_to_join_of_lower_bounds st cs gv =
  let lower_bound = List.fold_left (join_lower_bound gv) T.Effct.pure cs in
  set_gvar st gv lower_bound

let rule_move_down_variant st cs =
  let variant_nb =
    st.rhs_gvars
    |> Fun.flip T.GVar.Set.diff st.ngvs
    |> Fun.flip T.GVar.Set.diff st.pgvs
    |> Fun.flip T.GVar.Set.diff st.lhs_gvars
  in
  T.GVar.Set.iter (set_to_join_of_lower_bounds st cs) variant_nb;
  cs

(* ========================================================================= *)
(* Rule: remove redundant constraints *)

(** Removes constraints that are implied by other constraints. *)

(** Check if effect [eff1] is trivially a subeffect of [eff2]. *)
let trivial_subeffect eff1 eff2 =
  let (tvs1, gvs1) = T.Effct.view eff1 in
  List.for_all
    (fun (x, p1) ->
      IncrSAT.Formula.implies p1 (T.Effct.lookup_tvar eff2 x))
    tvs1 &&
  List.for_all
    (fun (gv, p1) ->
      IncrSAT.Formula.implies p1 (T.Effct.lookup_gvar eff2 gv))
    gvs1

(** Check if constraint [c1] implies constraint [c2]. It is assumed that
  both constraints have the same left-hand-side effect variable.

  Checking if constraint [x ? p1 ? ~n1 <: eff1] implies [x ? p2 ? ~n2 <: eff2]
  requires to check that:
  - [p2 and ~n2] implies [p1 and ~n1], which is equivalent to checking
    if [p2] implies [p1 or n2], and if [p2 and n1] implies [n2];
  - [eff1] is a trivial subeffect of [eff2]. *)
let constr_implies (c1 : constr) (c2 : constr) =
  IncrSAT.Formula.implies c2.pformula
    (IncrSAT.Formula.disj c1.pformula c2.nformula) &&
  IncrSAT.Formula.implies
    (IncrSAT.Formula.conj c2.pformula c1.nformula)
    c2.nformula &&
  trivial_subeffect c1.rhs_effect c2.rhs_effect

let add_if_not_redundant st acc c =
  match EffVarMap.find_opt c.eff_var acc with
  | None ->
    EffVarMap.add c.eff_var [c] acc
  | Some cs when List.exists (Fun.flip constr_implies c) cs ->
    (* Constraint [c] is redundant. *)
    st.progress <- true;
    acc
  | Some cs ->
    let not_redundant c' =
      if constr_implies c c' then begin
        st.progress <- true;
        false
      end else
        true
    in
    let cs = List.filter not_redundant cs in
    EffVarMap.add c.eff_var (c :: cs) acc

let rule_remove_redundant st cs =
  let cs_map = List.fold_left (add_if_not_redundant st) EffVarMap.empty cs in
  if st.progress then
    EffVarMap.fold (fun _ -> List.rev_append) cs_map []
  else
    cs

(* ========================================================================= *)
(* Rule combinators *)

let rec try_rules rules st cs =
  match rules with
  | [] -> cs
  | rule :: rules ->
    let cs = rule st cs in
    if st.progress then cs
    else try_rules rules st cs

(* ========================================================================= *)
(* Main loop of the simplification *)

let rules =
  try_rules [
    rule_close_positive;
    rule_move_up_negative;
    rule_move_down_variant;
    rule_remove_redundant;
  ]

let rec simplify_loop st cs =
  st.progress <- false;
  let cs = List.concat_map (normalize st) cs in
  st.lhs_gvars <- lhs_gvars st cs;
  st.rhs_gvars <- rhs_gvars st cs;
  let cs =
    cs
    |> rules st
    |> List.map to_constr
  in
  if st.progress then
    simplify_loop st cs
  else
    cs

let simplify ~scope ~pgvs ~ngvs cs =
  let st = initial_state ~scope ~pgvs ~ngvs in
  let cs = simplify_loop st cs in
  cs @ st.irr
