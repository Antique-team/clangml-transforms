(* author: francois.berenger@inria.fr

   // transform
   (e1, e2, e3);
   // into
   tmp0 = e1;
   tmp1 = e2;
   e3; *)

open Clang
open Ast
open Util.Prelude

(* collect expressions in the order they must be evaluated *)
let collect_exprs (expr: expr): expr list =
  let rec loop acc e = match e.e with
    | BinaryOperator (BO_Comma, e1, e2) -> loop (e2 :: acc) e1
    | _ -> e :: acc
  in
  loop [] expr

let transform_decl (_clang: Clang.Api.clang) =
  (* the usual map_stmt, since the map_expr after creates statements *)
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt ~replace:false map_stmt v state stmt stmts
    | _ ->
      MapVisitor.visit_stmt v state stmt
  (* the transform *)
  and map_expr
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (expr: expr): (stmt list) * expr =
    match expr.e with
    | BinaryOperator (BO_Comma, e1, e2) ->
      let collected = collect_exprs e1 in
      let new_state =
        List.fold_left
          (fun acc e ->
             let tmp_var = Codegen.fresh_varname "removeCommaBinop" in
             let decl_stmt =
               Codegen.declare_stmt ~init:e e.e_sloc tmp_var
                 (Types.tloc_of_ctyp e.e_sloc e.e_type)
             in
             (decl_stmt :: acc)
          )
          state
          collected
      in
      map_expr v new_state e2
    | _ -> MapVisitor.visit_expr v state expr (* default *)
  in
  let v = MapVisitor.({ default with map_expr; map_stmt }) in
  snd % MapVisitor.visit_decl v []
