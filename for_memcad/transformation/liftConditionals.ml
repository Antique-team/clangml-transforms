(* author Pippijn van Steenhoven <pip88nl@gmail.com>
   transform
   ---
   a = b ? c : d;
   ---
   into
   ---
   if (b)
     a = c;
   else
     a = d;
   ---
   and creates temporaries if 'a' is not a name *)
open Clang
open Util.Prelude

type state = {
  (* We keep the decls and stmts separate, so we can
     append only the stmts at the end of a loop, where
     we need to insert them twice (once before the loop
     and once after each iteration). *)
  inserted_decls : Ast.stmt list;
  inserted_stmts : Ast.stmt list;
}

let empty_state = {
  inserted_decls = [];
  inserted_stmts = [];
}


(* Clear inserted decls and stmts. *)
let clear_state () =
  { inserted_decls = [] ;
    inserted_stmts = [] }


let make_temporary state =
  let var = Codegen.fresh_varname "liftConditionals" in
  (state, var)


let transform_decl _clang =
  let open Ast in

  let rec map_expr v state expr =
    match expr.e with
    | ConditionalOperator (cond, then_expr, else_expr) ->
        let (state, var) = make_temporary state in

        let if_stmt = {
          s = IfStmt (
              cond,
              Codegen.assign_var_stmt var then_expr,
              Some (Codegen.assign_var_stmt var else_expr)
            );
          s_sloc = cond.e_sloc;
          s_cref = Ref.null;
        } in

        let var_decl =
          Codegen.declare_stmt cond.e_sloc var
            (Types.tloc_of_ctyp expr.e_sloc expr.e_type)
        in

        let (state, if_stmt) = map_stmt v state if_stmt in

        let inserted_stmts = if_stmt :: state.inserted_stmts in
        let inserted_decls = var_decl :: state.inserted_decls in

        let e = DeclRefExpr var in

        ({ inserted_stmts; inserted_decls },
         { expr with e })

    | StmtExpr body ->
        let saved_state = state in
        let (state, body) = map_stmt v (clear_state ()) body in
        assert (state.inserted_decls == []);
        assert (state.inserted_stmts == []);
        (saved_state,
         { expr with e = StmtExpr body })

    | _ ->
        MapVisitor.visit_expr v state expr


  and map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        (* There should be no unclaimed inserted decls/stmts. *)
        assert (state.inserted_decls == []);
        assert (state.inserted_stmts == []);

        (* We drop the state after the compound statement,
           so we can reuse temporary variable names. *)
        let _, stmts =
          List.fold_left (fun (state, stmts) stmt ->
            let (state, stmt) = map_stmt v state stmt in
            (clear_state (),
             stmt :: state.inserted_stmts @ state.inserted_decls @ stmts)
          ) (state, []) stmts
        in

        (state, { stmt with s = CompoundStmt (List.rev stmts) })

    | IfStmt (cond, then_stmt, else_stmt) ->
        let (state, cond) = map_expr v state cond in
        let then_stmt = map_sub_stmt v state then_stmt in
        let else_stmt = Util.Option.map (map_sub_stmt v state) else_stmt in

        (state, { stmt with s = IfStmt (cond, then_stmt, else_stmt) })

    | WhileStmt (cond, body) ->
        let (state, cond) = map_expr v state cond in
        let body = map_sub_stmt v state body in

        (* Execute the replacement code for the condition again
           after each iteration. *)
        let body = Codegen.append_stmts body state.inserted_stmts in

        (state, { stmt with s = WhileStmt (cond, body) })

    | SwitchStmt (cond, body) ->
        let (saved_state, cond) = map_expr v state cond in
        let (state, body) = map_stmt v (clear_state ()) body in
        assert (state.inserted_decls == []);
        assert (state.inserted_stmts == []);
        (saved_state,
         { stmt with s = SwitchStmt (cond, body) })

    | _ ->
        MapVisitor.visit_stmt v state stmt


  (* This function is used for statements with sub-statements.
     If we want to insert more statements into the sub-statement,
     we wrap the original sub-statement together with the added
     statements and declarations in a new compound statement.
     If no statements were added, we leave it alone. *)
  and map_sub_stmt v _state stmt =
    let (state, stmt) = map_stmt v (clear_state ()) stmt in

    match state.inserted_stmts, state.inserted_decls with
    | [], [] -> stmt
    | stmts, decls ->
        { s = CompoundStmt (List.rev @@ stmt :: stmts @ decls);
          s_sloc = stmt.s_sloc;
          s_cref = Ref.null;
        }


  and map_decl v state decl =
    match decl.d with
    | EnumConstantDecl (_name, _value) ->
        (* These might contain constant conditionals. We should
           compute them here or in another constant computation
           pass. *)
        (state, decl)

    | _ ->
        MapVisitor.visit_decl v state decl


  in

  let v = MapVisitor.({
    default with
    map_decl;
    map_expr;
    map_stmt;
  }) in

  snd % MapVisitor.visit_decl v empty_state
