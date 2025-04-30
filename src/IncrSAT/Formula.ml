(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Positive formulas *)

(** Conjunctions of variables *)
module Conj = PropVar.Set

(** Sets of conjunctions. Formulae are in DNF. *)
module ConjSet = Set.Make(Conj)

type t = ConjSet.t BRef.t

let top_data = ConjSet.singleton PropVar.Set.empty

let top = BRef.create top_data

let bot = BRef.create ConjSet.empty

let var x = BRef.create (ConjSet.singleton (PropVar.Set.singleton x))

let fresh_var () = var (PropVar.fresh ())

(* ========================================================================= *)

let simplify_conj conj =
  let rec loop acc xs =
    match xs with
    | [] -> Some acc
    | x :: xs ->
      begin match PropVar.value x with
      | True     -> loop acc xs
      | False    -> None
      | SameAs x -> loop (PropVar.Set.add x acc) xs
      end
  in
  loop PropVar.Set.empty (PropVar.Set.elements conj)

let fast_simplify data =
  let rec loop acc conjs =
    match conjs with
    | [] -> acc
    | conj :: conjs ->
      begin match simplify_conj conj with
      | None      -> loop acc conjs
      | Some conj -> loop (ConjSet.add conj acc) conjs
      end
  in
  loop ConjSet.empty (ConjSet.elements data)

let subsumed conj data =
  ConjSet.mem conj data ||
  ConjSet.exists (Fun.flip PropVar.Set.subset conj) data

let full_simplify data =
  let rec loop acc conjs =
    match conjs with
    | [] -> acc
    | conj :: conjs ->
      begin match simplify_conj conj with
      | None      -> loop acc conjs
      | Some conj ->
        if subsumed conj acc then loop acc conjs
        else
          let acc =
            ConjSet.filter
              (fun conj' -> not (PropVar.Set.subset conj conj'))
              acc
          in
          loop (ConjSet.add conj acc) conjs
      end
  in
  loop ConjSet.empty (ConjSet.elements data)

(* ========================================================================= *)

type bool3 = True | Unknown | False

let value f =
  let data = fast_simplify (BRef.get f) in
  if ConjSet.mem PropVar.Set.empty data then begin
    BRef.set f top_data;
    True
  end else begin
    BRef.set f data;
    if ConjSet.is_empty data then False
    else Unknown
  end

let is_true f =
  match value f with
  | True -> true
  | _    -> false

let is_false f =
  match value f with
  | False -> true
  | _     -> false

let implies f1 f2 =
  let data1 = fast_simplify (BRef.get f1) in
  let data2 = full_simplify (BRef.get f2) in
  BRef.set f1 data1;
  BRef.set f2 data2;
  ConjSet.for_all (fun conj -> subsumed conj data2) data1

let conj f1 f2 =
  match value f1 with
  | True  -> f2
  | False -> bot
  | Unknown ->
    begin match value f2 with
    | True    -> f1
    | False   -> bot
    | Unknown ->
      let f1 = BRef.get f1 in
      let f2 = BRef.get f2 in
      BRef.create
        (ConjSet.fold
          (fun c1 ->
            ConjSet.fold
              (fun c2 -> ConjSet.add (PropVar.Set.union c1 c2))
              f2)
          f1
          ConjSet.empty)
    end

let disj f1 f2 =
  match value f1 with
  | True  -> top
  | False -> f2
  | Unknown ->
    begin match value f2 with
    | True    -> top
    | False   -> f1
    | Unknown -> BRef.create (ConjSet.union (BRef.get f1) (BRef.get f2))
    end

(* ========================================================================= *)

let fix_conj conj =
  match simplify_conj conj with
  | None -> false
  | Some conj ->
    begin match PropVar.Set.choose_opt conj with
    | None -> true
    | Some x ->
      PropVar.set_bool x false;
      false
    end

let fix_conjs conjs =
  ConjSet.exists fix_conj conjs

let fix f =
  fix_conjs (full_simplify (BRef.get f))

(* ========================================================================= *)

let imp_var_to_cnf f x =
  let data = full_simplify (BRef.get f) in
  BRef.set f data;
  List.map
    (fun conj ->
      (x, true) :: List.map (fun y -> (y, false)) (PropVar.Set.elements conj))
    (ConjSet.elements data)

let var_imp_to_cnf x f =
  let data = full_simplify (BRef.get f) in
  BRef.set f data;
  let var_cls =
    List.map
      (fun conj ->
        let y   = PropVar.fresh () in
        let cls =
          List.map (fun z -> [(y, false); (z, true)])
            (PropVar.Set.elements conj) in
        (y, cls))
      (ConjSet.elements data)
  in
  ((x, false) :: List.map (fun (y, _) -> (y, true)) var_cls) ::
    List.concat_map snd var_cls

let imp_to_cnf f1 f2 =
  let x0 = PropVar.fresh () in
  imp_var_to_cnf f1 x0 @
  var_imp_to_cnf x0 f2

(* ========================================================================= *)

let conj_to_sexpr conj =
  match PropVar.Set.elements conj with
  | []  -> SExpr.Sym "true"
  | [x] -> SExpr.List [Sym "var"; PropVar.to_sexpr x]
  | xs  -> SExpr.List (Sym "and" :: List.map PropVar.to_sexpr xs)

let to_sexpr f =
  let data = full_simplify (BRef.get f) in
  BRef.set f data;
  match ConjSet.elements data with
  | []     -> SExpr.Sym "false"
  | [conj] -> conj_to_sexpr conj
  | conjs  -> SExpr.List (Sym "or" :: List.map conj_to_sexpr conjs)
