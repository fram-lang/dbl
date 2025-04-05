(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of datatype definitions from ConE to Core *)

open Common

(** Half-translated data-like definition *)
type data_def =
  | DD_Data of
    { tvar     : T.TVar.ex;
      proof    : S.var;
      args     : S.named_tvar list;
      ctors    : S.ctor_decl list;
      positive : bool;
    }

  | DD_Label of
    { tvar      : T.keffect T.tvar;
      var       : S.var;
      delim_tp  : S.typ;
      delim_eff : S.effct
    }

(** Translate a constructor declaration *)
let tr_ctor_decl env (ctor : S.ctor_decl) =
  let (env, tvars) = Env.add_named_tvars env ctor.ctor_targs in
  { T.ctor_name      = ctor.ctor_name;
    T.ctor_tvars     = tvars;
    T.ctor_arg_types =
      List.map (fun (_, sch) -> Type.tr_scheme env sch) ctor.ctor_named @
      List.map (Type.tr_scheme env) ctor.ctor_arg_schemes
  }

(** Translate a list of constructor declarations *)
let tr_ctor_decls env decls =
  List.map (tr_ctor_decl env) decls

let prepare_data_def env (dd : S.data_def) =
  match dd with
  | DD_Data dd ->
    let (env, tvar) = Env.add_tvar env dd.tvar in
    let dd = DD_Data
      { tvar;
        proof    = dd.proof;
        args     = dd.args;
        ctors    = dd.ctors;
        positive = dd.positive
      } in
    (env, dd)

  | DD_Label dd ->
    let (env, Ex tvar) = Env.add_tvar env dd.tvar in
    begin match T.TVar.kind tvar with
    | KEffect ->
      let dd = DD_Label {
          tvar; var = dd.var;
          delim_tp = dd.delim_tp; delim_eff = dd.delim_eff 
        }
      in
      (env, dd)
    | KType | KArrow _ -> failwith "Internal kind error"
    end

let finalize_data_def env (dd : data_def) =
  match dd with
  | DD_Data dd ->
    let (env, args) = Env.add_named_tvars env dd.args in
    let ctors = tr_ctor_decls env dd.ctors in
    T.DD_Data {
      tvar      = dd.tvar;
      proof     = dd.proof;
      args      = args;
      ctors     = ctors;
      positive  = dd.positive;
    }

  | DD_Label dd ->
    T.DD_Label {
      tvar      = dd.tvar;
      var       = dd.var;
      tvars     = [];
      val_types = [];
      delim_tp  = Type.tr_ttype  env dd.delim_tp;
      delim_eff = Type.tr_ceffect env (Impure dd.delim_eff)
    }

let tr_data_defs env dds =
  let (env, dds) = List.fold_left_map prepare_data_def env dds in
  (env, List.map (finalize_data_def env) dds)
