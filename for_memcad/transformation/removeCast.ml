(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr
   synopsis: remove implicit and explicit casts *)

open Clang
open Util.Prelude
open Ast

let transform_decl _clang =
  let rec map_expr
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (expr: expr): (stmt list) * expr =
    match expr.e with
    | ParenExpr e | ImplicitCastExpr (_, e) ->
      (* Assign the conversion type to the subexpression.
           E.g. this changes the type of [( void * )0] in
             [int *a = (( void * )0);]
           from [void *] to [int *].
      *)
      map_expr v state { e with e_type = expr.e_type }
    | CStyleCastExpr
        (_,
         { tl = PointerTypeLoc { tl = BuiltinTypeLoc BT_Void } },
         { e = IntegerLiteral 0 }) ->
      (* this case is handled specifically in memcad's main/transform.ml *)
      MapVisitor.visit_expr v state expr (* default *)
    | CStyleCastExpr (_, tloc, e) ->
      map_expr v state { e with e_type = tloc.tl_type }
    | _ -> MapVisitor.visit_expr v state expr (* default *)
  in
  let v = MapVisitor.({ default with map_expr }) in
  snd % MapVisitor.visit_decl v []
