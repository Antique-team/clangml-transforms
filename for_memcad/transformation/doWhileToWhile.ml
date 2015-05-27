(* author: francois.berenger@inria.fr *)

open Clang
open Ast
open Util.Prelude

(* // transform
do {
  body
} while (cond);
// into
body;
while (cond) {
  body;
} // *)

let transform_decl (_: Clang.Api.clang) =
  let rec map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt v state stmt stmts
    | DoStmt (body, cond) ->
        let while_loop = { stmt with s = WhileStmt (cond, body) } in
        let do_while = { stmt with s = CompoundStmt [body ; while_loop] } in
        map_stmt v state do_while
    | _ ->
        MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
