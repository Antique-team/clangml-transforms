(* author: francois.berenger@inria.fr
   // transform
   if ( x = f() ) ...
   // into
   x = f();
   if ( x ) ...
   // also handles while ( x = f() ) { ... } and
   // if ((x = f()) <= y) ... *)

open Clang
open Ast
open Util.Prelude
open Printf

(* true <=> e1 is a DeclRefExpr and e2 is a CallExpr *)
let decl_ref_and_call_expr (e1: expr) (e2: expr): bool =
  match Tests.get_decl_ref_expr e1 with
  | `None -> false
  | `Decl_ref_expr _ ->
    match Tests.get_call_expr e2 with
    | `None -> false
    | `Call_expr (_callee, _args) -> true

let handle_if_stmt (s: stmt): (stmt * stmt) option =
  match Tests.get_if_stmt s with
  | `None -> None
  | `If_stmt (cond, then_stmt, else_stmt) ->
    match Tests.get_assign cond with
    | `None -> (* assign can be deeper *)
      begin match Tests.get_relat_or_equal_binop cond with
        | `None -> None
        | `Relat_or_equal_binop (op, e1, e2) ->
          begin match (Tests.get_assign e1, Tests.get_assign e2) with
            | `None, `None -> None (* nothing to do *)
            | `Assign (_, _), `Assign (_, _) -> None (* not handled *)
            | `Assign (e11, e12), `None ->
              if not (decl_ref_and_call_expr e11 e12) then None
              else
                let assign_stmt = Codegen.stmt_of_expr e1 in
                let new_cond =
                  { cond with e = BinaryOperator (op, e11, e2) }
                in
                let new_if_stmt =
                  { s with s = IfStmt (new_cond, then_stmt, else_stmt) }
                in
                Some (assign_stmt, new_if_stmt)
            | `None, `Assign (e21, e22) ->
              if not (decl_ref_and_call_expr e21 e22) then None
              else
                let assign_stmt = Codegen.stmt_of_expr e2 in
                let new_cond =
                  { cond with e = BinaryOperator (op, e1, e21) }
                in
                let new_if_stmt =
                  { s with s = IfStmt (new_cond, then_stmt, else_stmt) }
                in
                Some (assign_stmt, new_if_stmt)
          end
      end
    | `Assign (e1, e2) ->
      if not (decl_ref_and_call_expr e1 e2) then None
      else
        let assign_stmt = Codegen.stmt_of_expr cond in
        let new_if_stmt = { s with s = IfStmt (e1, then_stmt, else_stmt) } in
        Some (assign_stmt, new_if_stmt)

(* this also forbids to have a continue stmt inside the while body because
   a continue (after this transformation) would not update the variable
   holding the condition
   --> FBR: TODO we have to put in place a transfo to check that no such cases
            are left behind *)
let handle_while_stmt (s: stmt): (stmt * stmt) option =
  match Tests.get_while_stmt s with
  | `None -> None
  | `While_stmt (cond, body) ->
    match Tests.get_assign cond with
    | `None ->
      begin match Tests.get_relat_or_equal_binop cond with
        | `None -> None
        | `Relat_or_equal_binop (op, e1, e2) ->
          begin match (Tests.get_assign e1, Tests.get_assign e2) with
            | `None, `None -> None (* nothing to do *)
            | `Assign (_, _), `Assign (_, _) -> None (* not handled *)
            | `Assign (e11, e12), `None -> (* assign in e1: e11 = e12; *)
              if not (decl_ref_and_call_expr e11 e12) then None
              else
                (* init before while *)
                let assign_stmt = Codegen.stmt_of_expr e1 in
                let new_cond = { cond with e = BinaryOperator (op, e11, e2) } in
                (* append update of variable at the end of the while body *)
                let new_body =
                  Codegen.compound_stmt body assign_stmt body.s_sloc
                in
                let new_while_stmt =
                  { s with s = WhileStmt (new_cond, new_body) }
                in
                Some (assign_stmt, new_while_stmt)
            | `None, `Assign (e21, e22) -> (* assign in e2: e21 = e11; *)
              if not (decl_ref_and_call_expr e21 e22) then None
              else
                let assign_stmt = Codegen.stmt_of_expr e2 in
                let new_cond = { cond with e = BinaryOperator (op, e1, e21) } in
                let new_body =
                  Codegen.compound_stmt body assign_stmt body.s_sloc
                in
                let new_while_stmt =
                  { s with s = WhileStmt (new_cond, new_body) }
                in
                Some (assign_stmt, new_while_stmt)
          end
      end
    | `Assign (e1, e2) ->
      if not (decl_ref_and_call_expr e1 e2) then None
      else
        let assign_stmt = Codegen.stmt_of_expr cond in
        let new_body = Codegen.compound_stmt body assign_stmt body.s_sloc in
        let new_while_stmt = { s with s = WhileStmt (e1, new_body) } in
        Some (assign_stmt, new_while_stmt)

let transform_decl (_clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt ~replace:false map_stmt v state stmt stmts
    | _ ->
      let maybe_stmts = match handle_if_stmt stmt with
        | Some x -> Some x
        | None -> handle_while_stmt stmt
      in
      match maybe_stmts with
      | None -> MapVisitor.visit_stmt v state stmt (* default *)
      | Some (assign, modified) ->
        let new_stmt = Codegen.compound_stmt assign modified stmt.s_sloc in
        map_stmt v state new_stmt
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
