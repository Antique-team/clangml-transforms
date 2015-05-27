(* author: francois.berenger@inria.fr *)

(* for loops are transformed into while loops by the ForToWhile transformation.
   Since a continue statement doesn't do the same thing in a for loop and in
   a while loop, the current transformation detects continue statements inside
   a for (with increment) loop body and raise an exception *)

open Clang
open Util.Prelude
open Ast

exception Continue_inside_for_with_incr

let create_map_visitor map_stmt_f =
  MapVisitor.({ default with map_stmt = map_stmt_f })

let transform_decl (_clang: Clang.Api.clang) =
  let rec map_stmt
      (inside_for_body: bool)
      (_v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    let map_f = map_stmt inside_for_body in
    let v = create_map_visitor map_f in
    match stmt.s with
    | CompoundStmt stmts ->
      MapStmt.mapCompoundStmt map_f v state stmt stmts
    | ForStmt (_, _, Some _incr, body) ->
      let v = create_map_visitor (map_stmt true) in
      (* inspect body *)
      let _ = map_stmt true v state body in
      (state, stmt)
    | ContinueStmt when inside_for_body ->
      raise Continue_inside_for_with_incr
    | _ ->
      MapVisitor.visit_stmt v state stmt (* default *)
  in
  let v = create_map_visitor (map_stmt false) in
  snd % MapVisitor.visit_decl v []
