(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr *)

open Clang
open Util.Prelude
open Ast

let transform_decl (clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | ForStmt (maybe_init, maybe_cond, maybe_incr, body) ->
      let cond = match maybe_cond with
        | Some c -> c (* unchanged *)
        | None ->
          let always_true = Codegen.integer_literal_expr clang stmt.s_sloc 1 in
          always_true
      in
      let while_body = match maybe_incr with
        | None -> body (* unchanged *)
        | Some incr ->
          let incr_stmt = Codegen.stmt_of_expr incr in
          Codegen.compound_stmt body incr_stmt body.s_sloc
      in
      let while_stmt = Codegen.while_stmt cond while_body stmt.s_sloc in
      let init_then_while = match maybe_init with
        | None -> while_stmt
        | Some init -> Codegen.compound_stmt init while_stmt stmt.s_sloc
      in
      (* recurse since there may be a for loop in the while's body *)
      map_stmt v state init_then_while
    | _ -> MapVisitor.visit_stmt v state stmt
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
