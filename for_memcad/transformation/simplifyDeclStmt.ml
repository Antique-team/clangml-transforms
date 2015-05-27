(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr *)

open Clang
open Util.Prelude
open Ast

let transform_decl _clang =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts -> MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | DeclStmt decls ->
        let replacement = List.rev_map Codegen.stmt_of_decl decls in
        (replacement, stmt) (* recursion stop *)
    | _ -> MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
