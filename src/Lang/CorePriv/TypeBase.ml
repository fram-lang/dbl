(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds and types *)

type ktype   = Dummy_Ktype
type keffect = Dummy_Keffect

type _ kind =
  | KType   : ktype kind
  | KEffect : keffect kind
  | KArrow  : 'k1 kind * 'k2 kind -> ('k1 -> 'k2) kind

let rec kind_equal : type k1 k2. k1 kind -> k2 kind -> (k1, k2) Eq.t =
  fun k1 k2 ->
  match k1, k2 with
  | KType,   KType   -> Equal
  | KType,   _       -> NotEqual
  | KEffect, KEffect -> Equal
  | KEffect, _       -> NotEqual
  | KArrow(ka1, kv1), KArrow(ka2, kv2) ->
    begin match kind_equal ka1 ka2, kind_equal kv1 kv2 with
    | Equal, Equal -> Equal
    | _            -> NotEqual
    end
  | KArrow _, _ -> NotEqual

module TVar : sig
  type 'k t = private {
    uid  : UID.t;
    kind : 'k kind
  }

  val kind : 'k t -> 'k kind

  val fresh : 'k kind -> 'k t

  val clone : 'k t -> 'k t

  val equal : 'k t -> 'k t -> bool
  
  val hequal : 'k1 t -> 'k2 t -> ('k1, 'k2) Eq.t

  type ex = Ex : 'k t -> ex

  module Map : Map1.S with type 'k key = 'k t
  module Set = Map.Set
end = struct
  type 'k t = {
    uid  : UID.t;
    kind : 'k kind
  }

  let kind x = x.kind

  let fresh kind =
    { uid  = UID.fresh ()
    ; kind = kind
    }

  let clone x = fresh x.kind

  let equal x y = x == y

  let hequal (type k1 k2) (x : k1 t) (y : k2 t) : (k1, k2) Eq.t =
    if x.uid <> y.uid then NotEqual
    else kind_equal x.kind y.kind

  type ex = Ex : 'k t -> ex

  module Map = Map1.Make(struct
    type nonrec 'a t = 'a t
    let uid x = x.uid
    let equal = hequal
  end)
  module Set = Map.Set
end
type 'k tvar = 'k TVar.t

type _ typ =
  | TUVar    : UID.t * 'k kind -> 'k typ
  | TEffPure : keffect typ
  | TEffJoin : effect * effect -> keffect typ
  | TVar     : 'k tvar -> 'k typ
  | TArrow   : ttype * ttype * effect -> ktype typ
  | TForall  : 'k tvar * ttype -> ktype typ
  | TLabel   : effect * ttype * effect -> ktype typ
  | TData    : ttype * ctor_type list -> ktype typ
  | TApp     : ('k1 -> 'k2) typ * 'k1 typ -> 'k2 typ

and ttype  = ktype typ
and effect = keffect typ

and ctor_type = {
  ctor_name      : string;
  ctor_tvars     : TVar.ex list;
  ctor_arg_types : ttype list
}
