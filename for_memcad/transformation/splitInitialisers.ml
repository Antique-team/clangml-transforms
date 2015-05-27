(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr
   // transform
   int i = 1;
   // into
   int i;
   i = 1;
   // also transform
   fun_call(); // a function, not a procedure
   // into
   var v;
   v = fun_call();
*)

open Clang
open Clang.Ast
open Util.Prelude

let seq _f g =
  g()

(* return type of a function *)
let get_return_type (e: Clang.Ast.expr): Clang.Ast.ctyp =
  match e.e with
  | CallExpr (callee, _args) ->
    begin match callee.e_type.t with
      | PointerType { t = FunctionNoProtoType ctyp
                        | FunctionProtoType (ctyp, _) } -> ctyp
      | _ -> seq (Log.fatal "get_return_type: unexpected callee: %s"
                    (Pp.string_of_ctyp callee.e_type))
               (fun () -> exit 1)
    end
  | _ -> seq (Log.fatal "get_return_type: not a function call: %s"
                (Pp.string_of_expr_ e.e))
           (fun () -> exit 1)

let transform_decl _clang =
  let rec map_stmt v state stmt = match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | ExprStmt ({ e = CallExpr (_callee, _args); e_sloc } as fun_call) ->
      let retval_type = get_return_type fun_call in
      if retval_type.t = BuiltinType BT_Void then
        (* procedure call *)
        MapVisitor.visit_stmt v state stmt (* default *)
      else (* this is a function call whose retval is ignored:
              we introduce a new temporary variable to hold its result
              and then the current transform to split
              this newly introduced initialiser *)
        let tmp_var = Codegen.fresh_varname "splitInitialisers" in
        let decl_stmt =
          Codegen.declare_stmt ~init:fun_call e_sloc tmp_var
            (Types.tloc_of_ctyp e_sloc retval_type)
        in
        map_stmt v state decl_stmt
    | DeclStmt [{ d = VarDecl (ty, name, Some init) } as decl] ->
        let init_stmt = {
          s = ExprStmt {
              e = BinaryOperator (
                  BO_Assign,
                  { e = DeclRefExpr name;
                    e_sloc = decl.d_sloc;
                    e_type = ty.tl_type;
                    e_cref = Ref.null;
                  },
                  init
                );
              e_sloc = decl.d_sloc;
              e_type = init.e_type;
              e_cref = Ref.null;
            };
          s_sloc = init.e_sloc;
          s_cref = Ref.null }
        in
        let new_stmt =
          { stmt with s = DeclStmt [
               { decl with d = VarDecl (ty, name, None) } ] }
        in
        (init_stmt :: [ new_stmt ], stmt)
    | DeclStmt (_::_::_) ->
        failwith "SplitInitialisers cannot handle multiple declarations"
    | _ ->
        MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
