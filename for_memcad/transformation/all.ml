(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr
   Pipeline of AST transformations *)

open Util.Prelude

let log s f x =
  Log.info "%s" s;
  f x

let transform_decl clang decl =
  decl
  |> log "transformations start" (fun x -> x)
  |> log "RemoveCast" (* ! must be first ! *)
    (* some transformations are not triggered in case casts remain *)
    (RemoveCast.transform_decl clang)
  |> log "MoveFunCall" (MoveFunCall.transform_decl clang)
  |> log "NoContinueInFor" (* ! before ForToWhile ! *)
    (NoContinueInFor.transform_decl clang)
  |> log "RemoveCommaBinop" (RemoveCommaBinop.transform_decl clang)
  |> log "BreakOpAssign" (BreakOpAssign.transform_decl clang)
  |> log "BreakMultiAssign" (BreakMultiAssign.transform_decl clang)
  |> log "SimplifyIf" (SimplifyIf.transform_decl clang)
  |> log "ForToWhile" (ForToWhile.transform_decl clang)
  |> log "DoWhileToWhile" (DoWhileToWhile.transform_decl clang)
  |> log "SwitchToIf" (SwitchToIf.transform_decl clang)
  |> log "MoveAssign" (MoveAssign.transform_decl clang)
  |> log "ReplaceFunCall" (ReplaceFunCall.transform_decl clang)
  |> log "SimplifyDeclStmt" (SimplifyDeclStmt.transform_decl clang)
  |> log "LiftConditionals" (LiftConditionals.transform_decl clang)
  |> log "PostIncrDecr" (PostIncrDecr.transform_decl clang)
  |> log "PreIncrDecr" (* ! after PostIncrDecr ! *)
    (PreIncrDecr.transform_decl clang)
  |> log "SplitInitialisers" (SplitInitialisers.transform_decl clang)
  |> log "transformations end" (fun x -> x)
(*|> NameAnonymousTypes.transform_decl clang*)
