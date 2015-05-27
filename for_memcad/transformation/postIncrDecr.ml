(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr
   //transform 
   a++;
   // into
   tmp = a;
   ++a;
   "return" tmp *)

open Clang
open Util.Prelude
open Ast

let post_to_pre (op: unary_operator): unary_operator = match op with
  | UO_PostInc -> UO_PreInc
  | UO_PostDec -> UO_PreDec
  | _ -> assert(false)

let transform_decl _clang =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt ~replace:false map_stmt v state stmt stmts
    | _ ->
      MapVisitor.visit_stmt v state stmt
  and map_expr
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (expr: expr): (stmt list) * expr =
    match expr.e with
    | UnaryOperator ((UO_PostInc | UO_PostDec) as op, operand) ->
      (* store current a in a tmp var *)
      let tmp_var = Codegen.fresh_varname "postIncrDecr" in
      let sloc = expr.e_sloc in
      let decl_tmp_var =
        Codegen.declare_stmt ~init:operand operand.e_sloc tmp_var
          (Types.tloc_of_ctyp sloc expr.e_type)
      in
      (* we reuse the previous implementation of {++/--}a to do the rest *)
      let final_op = post_to_pre op in
      let new_expr = { expr with e = UnaryOperator (final_op, operand) } in
      let new_stmt = Codegen.stmt_of_expr new_expr in
      let to_return =
        Codegen.decl_ref_expr tmp_var sloc expr.e_type
      in
      map_expr v (new_stmt :: decl_tmp_var :: state) to_return
    | _ ->
      MapVisitor.visit_expr v state expr
  in
  let v = MapVisitor.({ default with map_expr; map_stmt }) in
  snd % MapVisitor.visit_decl v []
