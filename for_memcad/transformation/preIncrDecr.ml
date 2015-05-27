(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr
   ++a becomes a = a + 1
   --a becomes a = a - 1 *)

open Clang
open Util.Prelude
open Ast

let unary_to_binary (op: unary_operator): binary_operator = match op with
  | UO_PreInc -> BO_Add
  | UO_PreDec -> BO_Sub
  | _ -> assert(false)

let transform_decl (clang: Clang.Api.clang) =
  let rec map_expr
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (expr: expr): (stmt list) * expr =
    match expr.e with
    | UnaryOperator ((UO_PreInc | UO_PreDec) as op, operand) ->
      let final_op = unary_to_binary op in
      let one_lit = Codegen.integer_literal_expr clang expr.e_sloc 1 in
      let new_val_expr = Codegen.apply_op_expr final_op operand one_lit in
      let modify_a_expr = Codegen.assign_expr operand new_val_expr in
      map_expr v state modify_a_expr
    | _ -> MapVisitor.visit_expr v state expr
  in
  let v = MapVisitor.({ default with map_expr }) in
  snd % MapVisitor.visit_decl v []
