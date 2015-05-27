(* author: francois.berenger@inria.fr
   the identity transformation is provided for debugging / tests purpose,
   when one wants to know what happens when all transformations are disabled *)

open Clang
open Ast

let transform_decl (_clang: Clang.Api.clang) (decl: Clang.AstBridge.decl) =
  decl
