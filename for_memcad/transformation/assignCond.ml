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
let rec transform_test_expr negate clang e2 =
  match Tests.get_unary_not e2 with (* !(ptr == 0) *)
  | `Unary_not e -> transform_test_expr true clang e
  | `None ->
    let int_1 = Codegen.integer_literal_expr clang e2.e_sloc 1 in
    let int_0 = Codegen.integer_literal_expr clang e2.e_sloc 0 in
    match Tests.get_relat_or_equal_binop e2 with
    | `Relat_or_equal_binop (_op, _e3, _e4) -> (* (ptr == 0) *)
      Some { e2 with e = (if negate
                          then ConditionalOperator (e2, int_0, int_1)
                          else ConditionalOperator (e2, int_1, int_0)) }
    | `None ->
      match Tests.get_logic_binop e2 with
      | `LAnd (e1, e2) ->  (* (e1 && e2) --> ((e1) ? e2 : 0) *)
        assert(negate = false); (* not handled *)
        Some { e2 with e = ConditionalOperator (e1, e2, int_0) }
      | `LOr (e1, e2) -> (* (e1 || e2) --> ((e1) ? 1 : e2) *)
        assert(negate = false); (* not handled *)
        Some { e2 with e = ConditionalOperator (e1, int_1, e2) }
      | `None -> None

let transform_decl (clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts -> MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | ReturnStmt (Some e) ->
      begin match transform_test_expr false clang e with
        | None -> MapVisitor.visit_stmt v state stmt (* default *)
        | Some e' ->
          let tmp_var = Codegen.fresh_varname "assignCond" in
          let sloc = e.e_sloc in
          let tloc = Types.tloc_of_ctyp sloc (Codegen.int_type clang) in
          let decl_tmp_var =
            Codegen.declare_stmt ~init:e' sloc tmp_var tloc
          in
          let new_ret_val = Some { e with e = DeclRefExpr tmp_var } in
          let new_return_stmt = { stmt with s = ReturnStmt new_ret_val } in
          let new_stmt =
            Codegen.compound_stmt decl_tmp_var new_return_stmt sloc
          in
          map_stmt v state new_stmt
      end
    | ExprStmt e ->
      begin match Tests.get_assign e with
        | `None -> MapVisitor.visit_stmt v state stmt (* default *)
        | `Assign (e1, e2) -> (* int i = (ptr == 0); *)
          begin match transform_test_expr false clang e2 with
            | None -> MapVisitor.visit_stmt v state stmt (* default *)
            | Some new_e2 ->
              let new_e = { e with e = BinaryOperator (BO_Assign, e1, new_e2) } in
              let new_stmt = { stmt with s = ExprStmt new_e } in
              map_stmt v state new_stmt
          end
      end
    | DeclStmt [{ d = VarDecl (ty, name, Some init) } as decl] ->
      begin match transform_test_expr false clang init with
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
