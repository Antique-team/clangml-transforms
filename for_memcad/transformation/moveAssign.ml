(* author: francois.berenger@inria.fr
   // transform
   if ((x = y) <= z) {
     ...
   }
   // into
   x = y;
   if (x <= z) {
     ...
   }
   // transform
   while (x = y) {
     ...
   }
   // into
   x = y;
   while (x) {
     ...
     x = y;
   }
*)

open Clang
open Ast
open Util.Prelude
open Printf

module Cg = Codegen

(* FBR: manage while loops too
   - refactor to get new_cond et assign_stmt
 *)

(* extract the assignment from a condition and return the updated condition *)
let extract_assign_and_cond (cond: expr): (stmt * expr) option =
  match Tests.get_relat_or_equal_binop cond with
  | `None ->
    begin match Tests.get_assign cond with (* (a = b) *)
      | `None -> None
      | `Assign (lhs, _rhs) ->
        let assign_stmt = Cg.stmt_of_expr cond in
        Some (assign_stmt, lhs)
    end
  | `Relat_or_equal_binop (op, e1, e2) -> (* (a <= b) *)
    match Tests.get_assign e1 with (* (a = b) *)
    | `Assign (lhs, _rhs) ->
      let assign_stmt = Cg.stmt_of_expr e1 in
      let new_cond = Cg.update_expr cond (BinaryOperator (op, lhs, e2)) in
      Some (assign_stmt, new_cond)
    | `None ->
      match Tests.get_assign e2 with
      | `None -> None (* the case where both e1 and e2 have an assignment
                         is not handled *)
      | `Assign (lhs, _rhs) ->
        let assign_stmt = Cg.stmt_of_expr e2 in
        let new_cond = Cg.update_expr cond (BinaryOperator (op, e1, lhs)) in
        Some (assign_stmt, new_cond)

let move_assign_in_if_or_while (s: stmt): (stmt * stmt) option =
  match Tests.get_if_stmt s with
  | `If_stmt (cond, then_stmt, else_stmt) ->
    begin match extract_assign_and_cond cond with
      | None -> None
      | Some (assign_stmt, new_cond) ->
        let new_if = Cg.update_stmt s (IfStmt (new_cond, then_stmt, else_stmt)) in
        Some (assign_stmt, new_if)
    end
  | `None ->
    match Tests.get_while_stmt s with
    | `None -> None
    | `While_stmt (cond, body) ->
      match extract_assign_and_cond cond with
      | None -> None
      | Some (assign_stmt, new_cond) ->
        (* we are doomed if there is a continue in the while loop *)
        let new_body = Cg.update_stmt body (CompoundStmt [body; assign_stmt]) in
        let new_while = Cg.update_stmt s (WhileStmt (new_cond, new_body)) in
        Some (assign_stmt, new_while)

let transform_decl (_clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt ~replace:false map_stmt v state stmt stmts
    | _ ->
      match move_assign_in_if_or_while stmt with
      | None -> MapVisitor.visit_stmt v state stmt
      | Some (assign, new_if_or_while) ->
        let new_stmt =
          Cg.update_stmt stmt (CompoundStmt [assign; new_if_or_while])
        in
        map_stmt v state new_stmt
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
