(* author: francois.berenger@inria.fr *)

open Clang
open Ast
open Util.Prelude
open Tests

module L  = BatList
module MS = MapStmt

(*
// the C code
switch (c) {
    case 'a': a_stmts;
      break;
    case 'b': b_stmts;
    case 'c': c_stmts;
      break;
    default: d_stmts;
  }
// is transformed into
int tmp_from_previous_case_0 = 0;
char tmp_test_0 = c; // evaluate the switch expression (c) only once
if (tmp_from_previous_case_0 || (tmp_test_0 == 'a')) {
   tmp_from_previous_case_0 = 0;
   a_stmts;
}
if (tmp_from_previous_case_0 || (tmp_test_0 == 'b')) {
   tmp_from_previous_case_0 = 1;
   b_stmts;
}
if (tmp_from_previous_case_0 || (tmp_test_0 == 'c')) {
   tmp_from_previous_case_0 = 0;
   c_stmts;
}
if (tmp_from_previous_case_0 || (tmp_test_0 != 'a' && tmp_test_0 != 'b' && tmp_test_0 != 'c') {
   tmp_from_previous_case_0 = 1;
   d_stmts;
}

FBR: TODO: enforce there are no breaks deep inside some statements
*)

(* what we can encounter inside a switch body *)
type switch_case =
  (* not last case of the switch and no break/return at the end *)
  | Case_no_break of expr * stmt list
  (* last case of the switch or break/return at the end *)
  | Case_break of expr * stmt list
  (* default, but not last case of the switch and no break/return at the end *)
  | Default_no_break of stmt list
  (* default, last case of the switch and break/return at the end *)
  | Default_break of stmt list

let is_case_no_break = function
  | Case_no_break (_, _) -> true
  | _ -> false
let is_case_break = function
  | Case_break (_, _) -> true
  | _ -> false
let is_default_no_break = function
  | Default_no_break _ -> true
  | _ -> false
let is_default_break = function
  | Default_break _ -> true
  | _ -> false

let collect_conditions (l: switch_case list) =
  let rec loop acc = function
    | [] -> L.rev acc
    | x :: xs ->
      match x with
      | Case_break (e, _)
      | Case_no_break (e, _) -> loop (e :: acc) xs
      | Default_break _
      | Default_no_break _ -> loop acc xs
  in
  loop [] l

(* condition for the default case given conditions for all other cases *)
let create_default_cond
    (clang: Clang.Api.clang)
    (sloc: sloc)
    (switch_expr: expr)
    (conditions: expr list): expr =
  let rec loop curr_cond = function
    | [] -> curr_cond
    | c :: cs ->
      let not_this_case = Codegen.not_equal_expr clang switch_expr c in
      loop (Codegen.and_expr clang curr_cond not_this_case) cs
  in
  let always_true = Codegen.integer_literal_expr clang sloc 1 in
  loop always_true conditions

(* things we don't handle *)
exception Case_with_some_range_end
exception Switch_body_not_a_compound_stmt
exception Statements_before_first_case_or_default of string
exception Statements_after_last_break_out_of_a_case of string
exception Stmts_dont_start_with_case_or_default of string

let neither_break_nor_return s =
  not_break_stmt s && not_return_stmt s

(* inside of the switch body, statements after the last break and without
   a case or default are forbidden *)
let check_for_trailing_stmts (stmts: stmt list): unit =
  let rev_stmts = List.rev stmts in
  let after_last_break =
    L.rev (L.take_while neither_break_nor_return rev_stmts)
  in
  match after_last_break with
  | [] -> ()
  | s :: _ ->
    if is_case_stmt s || is_default_stmt s then
      ()
    else
      raise (Statements_after_last_break_out_of_a_case (Pp.string_of_stmt s))

let not_case_nor_default s =
  not_case_stmt s && not_default_stmt s

let not_case_or_default_or_break_or_return s =
  not_case_nor_default s && neither_break_nor_return s

let is_break_or_return s =
  is_break_stmt s || is_return_stmt s

(* inside of the switch body, statements before the first case
   or default are forbidden *)
let check_for_leading_stmts (stmts: stmt list): unit =
  let trash = L.take_while not_case_or_default_or_break_or_return stmts in
  match trash with
  | [] -> ()
  | x :: _ ->
    raise (Statements_before_first_case_or_default (Pp.string_of_stmt x))

(* we don't support all possible CaseStmt from the AST *)
let is_unsupported_case (s: stmt): bool = match s.s with
  | CaseStmt (_, Some _, _) -> true
  | _ -> false

let check_for_unsupported_case (stmts: stmt list): unit =
  if L.exists is_unsupported_case stmts
  then raise Case_with_some_range_end
  else ()

exception Statement_not_a_break_or_return

let convert_break_or_return s =
  if is_break_stmt s then None
  else if is_return_stmt s then Some s
  else raise Statement_not_a_break_or_return

let append_break_or_return s stmts =
  let converted = convert_break_or_return s in
  match converted with
  | None -> stmts (* erase break stmt *)
  | Some s -> stmts @ [s] (* keep return stmt *)

(* consume stmts and classify them, until a break or return or case or default
   is reached *)
let consume_stmts = function
  | [] -> failwith "consume_stmts: no statements"
  | s1 :: ss ->
    match get_case_stmt s1 with
    | `None ->
      begin match get_default_stmt s1 with
        | `Default_stmt s ->
          begin
            let stmts', rest =
              L.span not_case_or_default_or_break_or_return ss
            in
            let stmts = s :: stmts' in
            match rest with
            | [] -> (Default_break stmts, rest)
            | x :: xs ->
              if is_break_or_return x then
                (Default_break (append_break_or_return x stmts), xs)
              else
                (Default_no_break stmts, x :: xs)
          end
        | `None ->
          raise (Stmts_dont_start_with_case_or_default (Pp.string_of_stmt s1))
      end
    | `Case_stmt (_, Some _, _) -> raise Case_with_some_range_end
    | `Case_stmt (e, None, s) ->
      let stmts', rest = L.span not_case_or_default_or_break_or_return ss in
      let stmts = s :: stmts' in
      match rest with
      | [] -> (Case_break (e, stmts), [])
      | x :: xs ->
        if is_break_or_return x then
          (Case_break (e, append_break_or_return x stmts), xs)
        else
          (Case_no_break (e, stmts), x :: xs)

(* retrieve the statement inside a case or default *)
let access_case_or_default_stmt (s: stmt): stmt option =
  match s.s with
  | DefaultStmt def_s -> Some def_s
  | CaseStmt (_, _, case_s) -> Some case_s
  | _ -> None

(* replace the statement inside a case or default *)
let replace_inside_case_or_default
    (case_or_default: stmt)
    (replacement: stmt): stmt =
  match case_or_default.s with
  | DefaultStmt _ ->
    { case_or_default with s = DefaultStmt replacement }
  | CaseStmt (a, b, _) ->
    { case_or_default with s = CaseStmt (a, b, replacement) }
  | _ -> assert(false)

(* convert
   1) a case followed by default into a case with a null stmt then default
   2) a default followed by a case into a default with a null stmt then case
   3) a case followed by another case into a case with a null stmt then the
      other case *)
let unfold_stmts (stmts: stmt list): stmt list =
  let rec loop acc = function
    | [] -> L.rev acc
    | s :: ss ->
      begin match access_case_or_default_stmt s with
        | None -> loop (s :: acc) ss (* consume s *)
        | Some stmt -> (* replace in s *)
          let null_stmt = Codegen.null_stmt s.s_sloc in
          let case_or_default_with_null = replace_inside_case_or_default s null_stmt in
          loop (case_or_default_with_null :: acc) (stmt :: ss)
      end
  in
  loop [] stmts

let classify_stmts stmts =
  let rec loop acc = function
    | [] -> L.rev acc
    | l ->
      let s, rest = consume_stmts l in
      loop (s :: acc) rest
  in
  loop [] (unfold_stmts stmts)

let switch_case_to_if
    (clang: Clang.Api.clang)
    (sloc: sloc)
    (default_cond: expr)
    (switch_var: expr)
    (from_previous: string * expr)
    (c: switch_case): stmt =
  (* if-condition corresponding to a case *)
  let case_if_cond e =
    (* if (tmp_from_previous_case_0 || (tmp_test_0 == 'b')) *)
    let cond = Codegen.equal_expr clang switch_var e in
    Codegen.or_expr clang (snd from_previous) cond
  in
  (* if-condition corresponding to a default *)
  let default_if_cond =
    (* if (tmp_from_previous_case_0 || default_cond) *)
    Codegen.or_expr clang (snd from_previous) default_cond
  in
  (* if-statements if break/return at the end of a case/default *)
  let break_if_stmts stmts =
    (* tmp_from_previous_case_0 = 0;
       other_stmts; *)
    (Codegen.reset_bool_stmt clang sloc (fst from_previous)) :: stmts
  in
  (* if-statements if no break/return at the end of a case/default *)
  let no_break_if_stmts stmts =
    (* tmp_from_previous_case_0 = 1;
       other_stmts; *)
    (Codegen.set_bool_stmt clang sloc (fst from_previous)) :: stmts
  in
  let cond, stmts = match c with
    | Case_no_break (e, stmts) -> (case_if_cond    e, no_break_if_stmts stmts)
    | Case_break    (e, stmts) -> (case_if_cond    e,    break_if_stmts stmts)
    | Default_no_break stmts   -> (default_if_cond  , no_break_if_stmts stmts)
    | Default_break    stmts   -> (default_if_cond  ,    break_if_stmts stmts)
  in
  let body = Codegen.compound_stmt_of_stmts stmts sloc in
  Codegen.if_stmt cond body sloc

(* print out a "diagnosis" of the switch being transformed *)
let quick_check (classified_stmts: switch_case list): unit =
  let cases_no_break   = L.filter is_case_no_break    classified_stmts in
  let cases_break      = L.filter is_case_break       classified_stmts in
  let default_no_break = L.filter is_default_no_break classified_stmts in
  let default_break    = L.filter is_default_break    classified_stmts in
  Log.info "%d case_no_break"    (L.length cases_no_break);
  Log.info "%d case_break"       (L.length cases_break);
  Log.info "%d default_no_break" (L.length default_no_break);
  Log.info "%d default_break"    (L.length default_break)

(* helper to prepare param for to_if_stmt *)
let prepare_var (name: string) (sloc: sloc) (ctyp: ctyp): string * expr =
  (name, Codegen.decl_ref_expr name sloc ctyp)

let transform_decl (clang: Clang.Api.clang) =
  let rec map_stmt
      (v: stmt list MapVisitor.visitor)
      (state: stmt list)
      (stmt: stmt): (stmt list) * stmt =
    match get_compound_stmt stmt with
    | `Compound_stmt stmts -> MS.mapCompoundStmt map_stmt v state stmt stmts
    | `None ->
      match get_switch_stmt stmt with
      | `None -> MapVisitor.visit_stmt v state stmt (* default *)
      | `Switch_stmt (expr, body) ->
        (* eval switch expression only once and store its result for later *)
        let switch_expr_varname = Codegen.fresh_varname "switchToIf" in
        let esloc1 = expr.e_sloc in
        let etype1 = expr.e_type in
        let tloc1 = Types.tloc_of_ctyp esloc1 etype1 in
        let switch_expr_var =
          Codegen.declare_stmt
            ~init:expr esloc1 switch_expr_varname tloc1
        in
        let _, switch_var = prepare_var switch_expr_varname esloc1 etype1 in
        (* from previous case variable *)
        let from_previous_case_varname = Codegen.fresh_varname "switchToIf" in
        let int_zero = Codegen.integer_literal_expr clang esloc1 0 in
        let etype2 = int_zero.e_type in
        let from_previous_var =
          let tloc2 = Types.tloc_of_ctyp esloc1 etype2 in
          Codegen.declare_stmt
            ~init:int_zero esloc1 from_previous_case_varname tloc2
        in
        let from_previous =
          prepare_var from_previous_case_varname esloc1 etype2
        in
        (* transform the switch body into ifs *)
        match get_compound_stmt body with
        | `None -> raise Switch_body_not_a_compound_stmt
        | `Compound_stmt stmts ->
          check_for_trailing_stmts stmts;
          check_for_unsupported_case stmts;
          check_for_leading_stmts stmts;
          let classified = classify_stmts stmts in
          (* quick_check classified; *)
          let sloc = stmt.s_sloc in
          let conditions = collect_conditions classified in
          let default_cond = create_default_cond clang sloc switch_var conditions in
          let ifs =
            List.map
              (switch_case_to_if clang sloc default_cond switch_var from_previous)
              classified
          in
          let stmts = switch_expr_var :: from_previous_var :: ifs in
          let compound_stmt = Codegen.compound_stmt_of_stmts stmts sloc in
          map_stmt v state compound_stmt
  in
  let v = MapVisitor.({ default with map_stmt }) in
  snd % MapVisitor.visit_decl v []
