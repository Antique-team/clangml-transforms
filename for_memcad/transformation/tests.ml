(* author: francois.berenger@inria.fr
   helper functions to make imbricated pattern matchings more readable
   in transformations *)

open Clang
open Ast

(* statements -------------------------------------------------------------- *)

let get_if_stmt (s: stmt) = match s.s with
  | IfStmt (cond, then_stmt, else_stmt) ->
    let _ = Log.debug "get_if_stmt" in
    `If_stmt (cond, then_stmt, else_stmt)
  | _ -> `None

let get_return_stmt (s: stmt) = match s.s with
  | ReturnStmt e ->
    let _ = Log.debug "get_return_stmt" in
    `Return_stmt e
  | _ -> `None

let is_return_stmt (s: stmt) = match get_return_stmt s with
  | `None -> false
  | _ -> true

let not_return_stmt s =
  not (is_return_stmt s)

let get_switch_stmt (s: stmt) = match s.s with
  | SwitchStmt (value, body) ->
    let _ = Log.debug "get_switch_stmt" in
    `Switch_stmt (value, body)
  | _ -> `None

let get_while_stmt (s: stmt) = match s.s with
  | WhileStmt (cond, body) ->
    let _ = Log.debug "get_while_stmt" in
    `While_stmt (cond, body)
  | _ -> `None

let get_compound_stmt (s: stmt) = match s.s with
  | CompoundStmt stmts ->
    let _ = Log.debug "get_compound_stmt" in
    `Compound_stmt stmts
  | _ -> `None

let get_case_stmt (s: stmt) = match s.s with
  | CaseStmt (range_begin, range_end, stmt) ->
    let _ = Log.debug "get_case_stmt" in
    `Case_stmt (range_begin, range_end, stmt)
  | _ -> `None

let is_case_stmt (s: stmt) = match get_case_stmt s with
  | `None -> false
  | _ -> true

let not_case_stmt s =
  not (is_case_stmt s)

let get_break_stmt (s: stmt) = match s.s with
  | BreakStmt ->
    let _ = Log.debug "get_break_stmt" in
    `Break_stmt
  | _ -> `None

let is_break_stmt (s: stmt) = match get_break_stmt s with
  | `None -> false
  | _ -> true

let not_break_stmt s =
  not (is_break_stmt s)

let get_default_stmt (s: stmt) = match s.s with
  | DefaultStmt s ->
    let _ = Log.debug "get_default_stmt" in
    `Default_stmt s
  | _ -> `None

let is_default_stmt (s: stmt) = match get_default_stmt s with
  | `None -> false
  | _ -> true

let not_default_stmt s =
  not (is_default_stmt s)

let get_continue_stmt (s: stmt) = match s.s with
  | ContinueStmt ->
    let _ = Log.debug "get_continue_stmt" in
    `Continue_stmt
  | _ -> `None

let is_continue_stmt (s: stmt) = match get_continue_stmt s with
  | `None -> false
  | _ -> true

(* expressions ------------------------------------------------------------- *)

let get_relat_or_equal_binop (e: expr) = match e.e with
  | BinaryOperator (op, e1, e2) ->
    begin match op with (* all relational and equality C99 operators *)
      | BO_LT | BO_GT | BO_LE | BO_GE | BO_EQ | BO_NE ->
        let _ = Log.debug "get_relat_or_equal_binop" in
        `Relat_or_equal_binop (op, e1, e2)
      | _ -> `None
    end
  | _ -> `None

let get_logic_binop (e: expr) = match e.e with
  | BinaryOperator (BO_LAnd, e1, e2) ->
    let _ = Log.debug "get_logic_binop: &&" in
    `LAnd (e1, e2)
  | BinaryOperator (BO_LOr, e1, e2) ->
    let _ = Log.debug "get_logic_binop: ||" in
    `LOr (e1, e2)
  | _ -> `None

let get_logical_and (e: expr) = match e.e with
  | BinaryOperator (BO_LAnd, e1, e2) ->
    let _ = Log.debug "get_logical_and" in
    `LAnd (e1, e2)
  | _ -> `None

let get_logical_or (e: expr) = match e.e with
  | BinaryOperator (BO_LOr, e1, e2) ->
    let _ = Log.debug "get_logical_or" in
    `LOr (e1, e2)
  | _ -> `None

let get_decl_ref_expr (e: expr) = match e.e with
  | DeclRefExpr str ->
    let _ = Log.debug "get_decl_ref_expr" in
    `Decl_ref_expr str
  | _ -> `None

let get_unary_not (e: expr) = match e.e with
  | UnaryOperator (UO_LNot, e) ->
    let _ = Log.debug "get_unary_not" in
    `Unary_not e
  | _ -> `None

let get_paren_expr (e: expr) = match e.e with
  | ParenExpr e1 ->
    let _ = Log.debug "get_paren_expr" in
    `Paren_expr e1
  | _ -> `None

let rec get_assign (e: expr) = match e.e with
  | BinaryOperator (BO_Assign, e1, e2) ->
    let _ = Log.debug "get_assign" in
    `Assign (e1, e2)
  | ParenExpr pe -> get_assign pe (* between parenthesis is also OK *)
  | _ -> `None

let is_assign (e: expr): bool = match get_assign e with
  | `None -> false
  | _ -> true

let rec get_call_expr (e: expr) = match e.e with
  | CallExpr (callee, args) ->
    let _ = Log.debug "get_call_expr" in
    `Call_expr (callee, args)
  | ParenExpr pe -> get_call_expr pe (* between parenthesis is also OK *)
  | _ -> `None

let is_a_call_expr (e: expr): bool = match get_call_expr e with
  | `None -> false
  | _ -> true
