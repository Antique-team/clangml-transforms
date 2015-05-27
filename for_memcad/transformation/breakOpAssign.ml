(* author: francois.berenger@inria.fr

   decompose combined operators doing an assignment:
   -------------------------------------------------
   addition assignment            a += b --> a = a + b
   subtraction assignment         a -= b --> a = a - b, etc.
   multitplication assignment     a *= b
   division assignment            a /= b
   modulo assignment              a %= b
   bitwise AND assignment         a &= b
   bitwise OR assignment          a |= b
   bitwise XOR assignment         a ^= b
   bitwise left shift assignment  a <<= b
   bitwise right shift assignment a >>= b *)

open Clang
open Ast
open Util.Prelude

exception Does_not_qualify

(* translate an assigning operator into its non assigning counterpart *)
let assign_to_no_assign (op: binary_operator): binary_operator =
  match op with
  | BO_MulAssign -> BO_Mul
  | BO_DivAssign -> BO_Div
  | BO_RemAssign -> BO_Rem
  | BO_AddAssign -> BO_Add
  | BO_SubAssign -> BO_Sub
  | BO_ShlAssign -> BO_Shl
  | BO_ShrAssign -> BO_Shr
  | BO_AndAssign -> BO_And
  | BO_OrAssign  -> BO_Or
  | BO_XorAssign -> BO_Xor
  | _ -> raise Does_not_qualify

(* is 'op' handled by this transformation ? *)
let op_qualifies (op: binary_operator): bool =
  try let _ = assign_to_no_assign op in true
  with Does_not_qualify -> false

let transform_decl (_clang: Clang.Api.clang) =
  let rec map_expr
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (expr: expr): (stmt list) * expr =
    match expr.e with
    | BinaryOperator (op, e1, e2) when op_qualifies op ->
      let new_op = assign_to_no_assign op in
      (* apply it *)
      let apply_expr = Codegen.apply_op_expr new_op e1 e2 in
      (* store result *)
      let store_expr = Codegen.assign_expr e1 apply_expr in
      map_expr v state store_expr
    | _ -> MapVisitor.visit_expr v state expr (* default *)
  in
  let v = MapVisitor.({ default with map_expr }) in
  snd % MapVisitor.visit_decl v []
