(* author: francois.berenger@inria.fr *)

open Clang
open Ast
open Util.Prelude
open Printf

(* 1) call the function and store the result into a temp. variable
   2) use that variable instead
   Examples of cases which are managed by this transformation:
    if ( ! f() ) ...
    if ( f() ) ...
    if ( f() == x ) ...
    if ( x == f() ) ...
    g(x, f(), z, ...) ... with any number of function calls in the args of g
    return f();
    return (f()); *)

let create_tmp_var (init: expr): (string * stmt) =
  let var_name = Codegen.fresh_varname "replaceFunCall" in
  let var_decl =
    Codegen.declare_stmt ~init:init init.e_sloc var_name
      (Types.tloc_of_ctyp init.e_sloc init.e_type)
  in
  (var_name, var_decl)

let get_call_expr_lhs_xor_rhs (e: expr) =
  match Tests.get_relat_or_equal_binop e with
  | `None -> `None
  | `Relat_or_equal_binop (op, e1, e2) ->
    match (Tests.get_call_expr e1, Tests.get_call_expr e2) with
    | `None       , `None        -> `None
    | `Call_expr _, `Call_expr _ -> `None (* not supported by transfo. *)
    | `Call_expr _, `None        -> `Call_expr_lhs (op, e1, e2)
    | `None       , `Call_expr _ -> `Call_expr_rhs (op, e1, e2)

let replace_call_by_tmp_var (s: stmt): (stmt * stmt_) option =
  match Tests.get_return_stmt s with
  | `Return_stmt None -> None
  | `Return_stmt (Some e) ->
    begin match Tests.get_call_expr e with
      | `None -> None
      | `Call_expr _ ->
        let _ = Log.debug "return f();" in
        let name, decl = create_tmp_var e in
        let new_ret_val = Some { e with e = DeclRefExpr name } in
        Some (decl, ReturnStmt new_ret_val)
    end
  | `None ->
    begin match Tests.get_if_stmt s with
      | `None -> None
      | `If_stmt (cond, then_stmt, else_stmt) ->
        begin match Tests.get_call_expr cond with
        | `Call_expr (_callee, _args) ->
          let _ = Log.debug "if ( f() )" in
          let name, decl = create_tmp_var cond in
          let new_cond = { cond with e = DeclRefExpr name } in
          Some (decl, IfStmt (new_cond, then_stmt, else_stmt))
        | `None ->
          begin match Tests.get_unary_not cond with
          | `Unary_not e ->
            begin match Tests.get_call_expr e with
              | `None -> None
              | `Call_expr (_callee, _args) ->
                let _ = Log.debug "if ( ! f() )" in
                let name, decl = create_tmp_var e in
                let new_cond =
                  Codegen.negate_expr { e with e = DeclRefExpr name }
                in
                Some (decl, IfStmt (new_cond, then_stmt, else_stmt))
            end
          | `None ->
            begin match get_call_expr_lhs_xor_rhs cond with
            | `None -> None
            | `Call_expr_lhs (op, e1, e2) ->
              let _ = Log.debug "if ( f() op x )" in
              let name, decl = create_tmp_var e1 in
              let new_e1 = { e1 with e = DeclRefExpr name } in
              let new_cond =
                { cond with e = BinaryOperator (op, new_e1, e2) }
              in
              Some (decl, IfStmt (new_cond, then_stmt, else_stmt))
            | `Call_expr_rhs (op, e1, e2) ->
              let _ = Log.debug "if ( x op f() )" in
              let name, decl = create_tmp_var e2 in
              let new_e2 = { e2 with e = DeclRefExpr name } in
              let new_cond =
                { cond with e = BinaryOperator (op, e1, new_e2) }
              in
              Some (decl, IfStmt (new_cond, then_stmt, else_stmt))
            end
          end
        end
    end

(* cut l in 3: ([elts before x], x | p(x) is true, [elts after x]) *)
let split_in_3 p l =
  let rec loop acc = function
    | [] -> failwith "replaceFunCall: split_in_3: no element satisfies p"
    | x :: xs ->
      if p x then (List.rev acc, x, xs)
      else loop (x :: acc) xs
  in loop [] l

(* replace the first function call found in the args by the tmp_var *)
let replace_one_call_by_tmp_var callee args = match args with
  | [] -> (None, CallExpr (callee, args))
  | _ ->
    let before, to_replace, after = split_in_3 Tests.is_a_call_expr args in
    let name, decl = create_tmp_var to_replace in
    let replacement = { to_replace with e = DeclRefExpr name } in
    (Some decl, CallExpr (callee, before @ (replacement :: after)))

(* each fun call found is replaced by a tmp_var and creates a decl *)
let process args =
  let rec loop decls_acc args_acc = function
    | [] -> (List.rev decls_acc, List.rev args_acc)
    | arg :: others ->
      if Tests.is_a_call_expr arg then
        let name, decl = create_tmp_var arg in
        let replacement = { arg with e = DeclRefExpr name } in
        loop (decl :: decls_acc) (replacement :: args_acc) others
      else
        loop decls_acc (arg :: args_acc) others
  in
  loop [] [] args

(* replace all function calls found in the args by tmp vars *)
let replace_all_calls_by_tmp_vars callee args =
  let decls, new_args = process args in
  (decls, CallExpr (callee, new_args))

let maybe_prepend (x_opt: 'a option) (l: 'a list): 'a list = match x_opt with
  | None -> l
  | Some x -> x :: l

let transform_decl (clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt ~replace:false map_stmt v state stmt stmts
    | _ ->
      begin match replace_call_by_tmp_var stmt with
        | None -> MapVisitor.visit_stmt v state stmt
        | Some (decl_tmp_var, new_stmt) ->
          let new_stmt' = Codegen.update_stmt stmt new_stmt in
          let compound_stmt =
            Codegen.compound_stmt decl_tmp_var new_stmt' stmt.s_sloc
          in
          map_stmt v state compound_stmt
      end
  and map_expr
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (expr: expr): (stmt list) * expr =
    match Tests.get_call_expr expr with
    | `None -> MapVisitor.visit_expr v state expr
    | `Call_expr (callee, args) ->
      let calls_in_args = List.filter Tests.is_a_call_expr args in
      begin match List.length calls_in_args with
        | 0 -> MapVisitor.visit_expr v state expr
        | 1 ->
          let decl_tmp_var, new_call_expr =
            replace_one_call_by_tmp_var callee args
          in
          let new_state = maybe_prepend decl_tmp_var state in
          map_expr v new_state { expr with e = new_call_expr }
        | _ ->
          let line_number = Clang_utils.line_of_sloc clang callee.e_sloc in
          let _ = Log.warn "replaceFunCall: line %d: left to right eval of \
                            fun calls in fun args"
              line_number
          in
          let decls, new_call_expr =
            replace_all_calls_by_tmp_vars callee args
          in
          (* statements are prepended in reverse order to the state;
             according to other transforms *)
          let new_state = (List.rev decls) @ state in
          map_expr v new_state { expr with e = new_call_expr }
      end
  in
  let v = MapVisitor.({ default with map_stmt; map_expr }) in
  snd % MapVisitor.visit_decl v []
