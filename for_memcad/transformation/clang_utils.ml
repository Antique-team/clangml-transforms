open Clang
open Ast

(* retrieve line number *)
let line_of_sloc (clang: Clang.Api.clang) (loc: Clang.Ast.sloc): int =
  Api.(request clang @@ PresumedLoc loc.loc_s).Sloc.loc_line
