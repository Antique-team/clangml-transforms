(* author: Pippijn van Steenhoven <pip88nl@gmail.com>
           and francois.berenger@inria.fr
   helper functions to write transformations *)

open Clang
open Ast

let null = Ref.null

let generated_vars = ref 0

exception Cannot_create_more_tmp_variables

(* create a fresh (unique) variable name, for temporary variables
   the transformations need to introduce *)
let fresh_varname (prfx: string): string =
  (* the leading '%' is to make sure this is not a regular C variable
     name the programmer could have used (it is an invalid name) *)
  let name = Printf.sprintf "%%%s_tmp_%d" prfx !generated_vars in
  if !generated_vars = -1 then
    (* would be very nasty if we don't detect that *)
    raise Cannot_create_more_tmp_variables
  else
    let _ = incr generated_vars in
    name

let int_type clang =
  Api.(request clang @@ CacheFor Cache_ctyp)
  |> Types.make_type_map
  |> Types.find_basic_int_type

let declare_stmt
    ?(init: expr option) (sloc: sloc) (var: string) (ty: tloc): stmt =
  let var_decl = VarDecl (ty, var, init) in
  let decl_stmt = DeclStmt [{ d      = var_decl ;
                              d_sloc = sloc     ;
                              d_cref = null     }]
  in
  { s      = decl_stmt ;
    s_sloc = sloc      ;
    s_cref = null      }

(* // create expr
   e1 op e2; *)
let apply_op_expr (op: binary_operator) (e1: expr) (e2: expr): expr =
  let new_expr = BinaryOperator (op, e1, e2) in
  { e1 with e      = new_expr ;
            e_cref = null     }

(* // create expr
   e1 = e2; *)
let assign_expr (e1: expr) (e2: expr): expr =
  apply_op_expr BO_Assign e1 e2

let stmt_of_expr (e: expr): stmt =
  { s      = ExprStmt e ;
    s_cref = null       ;
    s_sloc = e.e_sloc   }

let stmt_of_decl (d: decl): stmt =
  { s      = DeclStmt [d] ;
    s_cref = null         ;
    s_sloc = d.d_sloc     }

(* // create stmt
   e1 = e2; *)
let assign_stmt (e1: expr) (e2: expr): stmt =
  stmt_of_expr (assign_expr e1 e2)

let assign_var_stmt (var: string) (expr: expr): stmt =
  let decl_ref_expr = { expr with e      = DeclRefExpr var ;
                                  e_cref = null            }
  in
  assign_stmt decl_ref_expr expr

let append_stmts (stmt: stmt) (stmts: stmt list): stmt = match stmts with
  | [] -> stmt
  | stmts ->
    let compount_stmt = CompoundStmt (stmt :: stmts) in
    { stmt with s      = compount_stmt ;
                s_cref = null          }

let decl_ref_expr (name: string) (e_sloc: sloc) (e_type: ctyp): expr =
  { e      = DeclRefExpr name ;
    e_cref = null             ;
    e_sloc ; e_type           }

let negate_expr (e: expr): expr =
  let negated = UnaryOperator (UO_LNot, e) in
  { e with e      = negated ;
           e_cref = null    }

(* e --> (e) *)
let paren_expr (e1: expr): expr =
  { e1 with e = ParenExpr e1 }

(* (e1 op e2) *)
let binop_expr
    (clang: Clang.Api.clang) (op: binary_operator) (e1: expr) (e2: expr): expr =
  let test_expr = BinaryOperator (op, e1, e2) in
  paren_expr { e1 with e      = test_expr      ;
                       e_cref = null           ;
                       e_type = int_type clang }

(* (e1 == e2) *)
let equal_expr (clang: Clang.Api.clang) (e1: expr) (e2: expr): expr =
  binop_expr clang BO_EQ e1 e2

(* (e1 != e2) *)
let not_equal_expr (clang: Clang.Api.clang) (e1: expr) (e2: expr): expr =
  binop_expr clang BO_NE e1 e2

(* (e1 && e2) *)
let and_expr (clang: Clang.Api.clang) (e1: expr) (e2: expr): expr =
  binop_expr clang BO_LAnd e1 e2

(* (e1 || e2) *)
let or_expr (clang: Clang.Api.clang) (e1: expr) (e2: expr): expr =
  binop_expr clang BO_LOr e1 e2

(* an if (with no else) *)
let if_stmt (cond: expr) (body: stmt) (sloc: sloc): stmt =
  { s      = IfStmt (cond, body, None) ;
    s_sloc = sloc                      ;
    s_cref = null                      }

(* an if with and else) *)
let if_else_stmt
    (cond: expr) (then_body: stmt) (else_body: stmt option) (sloc: sloc)
  : stmt =
  { s      = IfStmt (cond, then_body, else_body) ;
    s_sloc = sloc                                ;
    s_cref = null                                }

let while_stmt (cond: expr) (body: stmt) (sloc: sloc): stmt =
  { s      = WhileStmt (cond, body) ;
    s_sloc = sloc                   ;
    s_cref = null                   }

let compound_stmt (s1: stmt) (s2: stmt) (sloc: sloc): stmt =
  { s      = CompoundStmt [s1; s2] ;
    s_sloc = sloc                  ;
    s_cref = null                  }

let update_stmt (s1: stmt) (s2: stmt_): stmt =
  { s1 with s = s2 }

let update_expr (e1: expr) (e2: expr_): expr =
  { e1 with e = e2 }

let null_stmt (sloc: sloc): stmt =
  { s      = NullStmt ;
    s_sloc = sloc     ;
    s_cref = null     }

let compound_stmt_of_stmts (stmts: stmt list) (sloc: sloc): stmt =
  { s      = CompoundStmt stmts ;
    s_sloc = sloc               ;
    s_cref = null               }

(* create an int of value 'i' as an expression *)
let integer_literal_expr (clang: Clang.Api.clang) (sloc: sloc) (i: int): expr =
  { e      = IntegerLiteral i ;
    e_type = int_type clang   ;
    e_sloc = sloc             ;
    e_cref = null             }

(* set int variable to 1 *)
let set_bool_stmt (clang: Clang.Api.clang) (sloc: sloc) (var: string): stmt =
  assign_var_stmt var (integer_literal_expr clang sloc 1)

(* set int variable to 0 *)
let reset_bool_stmt (clang: Clang.Api.clang) (sloc: sloc) (var: string): stmt =
  assign_var_stmt var (integer_literal_expr clang sloc 0)
