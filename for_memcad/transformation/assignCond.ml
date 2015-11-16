(* author: francois.berenger@inria.fr

   transform assignment of a condition into assignment
   of a conditional operator:
   --------------------------
   int i = (ptr == 0);
   // -->
   int i = (ptr == 0) ? 1 : 0;
   // and
   i = (ptr == 0);
   // -->
   i = (ptr == 0) ? 1 : 0;
*)

open Clang
open Ast
open Util.Prelude
open Tests

module L = BatList

(* (ptr == 0) --> ((ptr == 0) ? 1 : 0) *)
let rec transform_assign_expr negate clang e2 =
  match Tests.get_unary_not e2 with (* !(ptr == 0) *)
  | `Unary_not e -> transform_assign_expr true clang e
  | `None ->
    match Tests.get_relat_or_equal_binop e2 with
    | `None -> None
    | `Relat_or_equal_binop (_op, _e3, _e4) -> (* (ptr == 0) *)
      let int_1 = Codegen.integer_literal_expr clang e2.e_sloc 1 in
      let int_0 = Codegen.integer_literal_expr clang e2.e_sloc 0 in
      let new_e2 =
        if negate then
          { e2 with e = ConditionalOperator (e2, int_0, int_1) }
        else
          { e2 with e = ConditionalOperator (e2, int_1, int_0) }
      in
      Some new_e2

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
          begin match transform_assign_expr false clang e2 with
            | None -> MapVisitor.visit_stmt v state stmt (* default *)
            | Some new_e2 ->
              let new_e = { e with e = BinaryOperator (BO_Assign, e1, new_e2) } in
              let new_stmt = { stmt with s = ExprStmt new_e } in
              map_stmt v state new_stmt
          end
      end
    | DeclStmt [{ d = VarDecl (ty, name, Some init) } as decl] ->
      begin match transform_assign_expr false clang init with
        | None -> MapVisitor.visit_stmt v state stmt (* default *)
        | Some new_init ->
          let new_decl = { decl with d = VarDecl (ty, name, Some new_init) } in
          let new_stmt = { stmt with s = DeclStmt [new_decl] } in
          map_stmt v state new_stmt
      end
    | _ -> MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
