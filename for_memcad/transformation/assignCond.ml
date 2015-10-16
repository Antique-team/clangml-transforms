(* author: francois.berenger@inria.fr

   transform assignment of a condition into assignment
   of a conditional operator:
   --------------------------
   int i = (ptr == 0);
   // -->
   int i = (ptr == 0) ? 1 : 0;
*)

open Clang
open Ast
open Util.Prelude
open Tests

module L = BatList

let transform_decl (clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts -> MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | ExprStmt e ->
      begin match Tests.get_assign e with
        | `None -> MapVisitor.visit_stmt v state stmt (* default *)
        | `Assign (e1, e2) -> (* int i = (ptr == 0); *)
          begin match Tests.get_relat_or_equal_binop e2 with
            | `None -> MapVisitor.visit_stmt v state stmt (* default *)
            | `Relat_or_equal_binop (_op, _e3, _e4) -> (* (ptr == 0) *)
              let int_1 = Codegen.integer_literal_expr clang e2.e_sloc 1 in
              let int_0 = Codegen.integer_literal_expr clang e2.e_sloc 0 in
              let new_e2 = { e2 with e = ConditionalOperator (e2, int_1, int_0) } in
              let new_e = { e with e = BinaryOperator (BO_Assign, e1, new_e2) } in
              let new_stmt = { stmt with s = ExprStmt new_e } in
              map_stmt v state new_stmt
          end
      end
    | _ -> MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
