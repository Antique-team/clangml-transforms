(* author: francois.berenger@inria.fr
   // transform
   a = b = c = 1;
   // into
   c = 1;
   b = c;
   a = c;
*)

open Clang
open Ast
open Util.Prelude
open Tests

module L = BatList

(* a chain of assignments is at least of length 2; as in
   a = b = c; *)
let is_assign_chain (e: expr): bool =
  match e.e with
  | BinaryOperator (BO_Assign, _e1, e2) ->
    begin
      match e2.e with
      | BinaryOperator (BO_Assign, _, _) -> true
      | _ -> false
    end
  | _ -> false

(* return the reversed list of expressions that were lhs of assignments
   and the value that was rhs
   e.g.: dissect (a = (b = (c = 1))) = ([c; b; a], 1) *)
let dissect_assign_chain (expr: expr): expr list * expr =
  let rec loop acc e = match e.e with
    | BinaryOperator (BO_Assign, e1, e2) -> loop (e1 :: acc) e2
    | _ -> (acc, e)
  in
  loop [] expr

let unfold_assignments (expr: expr): stmt list =
  let rev_vars, init_expr = dissect_assign_chain expr in
  match rev_vars with
  | [] -> assert(false)
  | last_var :: others ->
    (* use the init_expr just once
       c = 1; *)
    let first_assign = Codegen.assign_stmt last_var init_expr in
    (* then reuse the var previously initialized
       b = c;
       a = c;
       [...] *)
    let other_assigns =
      L.map
        (fun v -> Codegen.assign_stmt v last_var)
        others
    in
    first_assign :: other_assigns

let transform_decl (_: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | ExprStmt e when is_assign_chain e ->
        let stmts = unfold_assignments e in
        let compound = Codegen.compound_stmt_of_stmts stmts e.e_sloc in
        map_stmt v state compound
    | _ ->
        MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
