(* author: francois.berenger@inria.fr
   // transform ----------------------
   if (a && b) {
     then_body;
   } else {
     else_body;
   }
   // into
   if (a) {
     if (b) {
       then_body;
     } else {
       else_body;
     }
   } else {
     else_body;
   }
   // transform ----------------------
   if (a || b) {
     then_body;
   } else {
     else_body;
   }
   // into
   if (a) {
     then_body;
   } else {
     if (b) {
       then_body;
     } else {
       else_body;
     }
   }
*)

open Clang
open Ast
open Util.Prelude

let transform_decl (_: Clang.Api.clang) =
  let rec map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts -> MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | _ ->
      match Tests.get_if_stmt stmt with
      | `None -> MapVisitor.visit_stmt v state stmt (* default *)
      | `If_stmt (cond, then_body, else_body) ->
        match Tests.get_logical_and cond with
        | `LAnd (a, b) ->
          let new_then_body =
            Codegen.if_else_stmt b then_body else_body then_body.s_sloc
          in
          let new_if_stmt =
            Codegen.if_else_stmt a new_then_body else_body stmt.s_sloc
          in
          map_stmt v state new_if_stmt
        | `None ->
          begin match Tests.get_logical_or cond with
          | `None -> MapVisitor.visit_stmt v state stmt (* default *)
          | `LOr (a, b) ->
            let new_else_body =
              Codegen.if_else_stmt b then_body else_body stmt.s_sloc
            in
            let new_if_stmt =
              Codegen.if_else_stmt a then_body (Some new_else_body) stmt.s_sloc
            in
            map_stmt v state new_if_stmt
          end
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
