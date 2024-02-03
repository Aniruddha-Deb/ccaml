open Lexer

(*

Rather than decoding the entire grammar at once, go step by step. Start with a 
simple function (int main) and then add complexity

file -> fn | fn_list fn 
fn -> return_type fn_name ( args_list ) compound_stmt

compound_stmt -> { stmt_list } 
stmt_list -> stmt ; | stmt_list stmt ;

stmt -> var_type var_name = add_sub_expr;

add_sub_expr -> mul_div_expr + add_sub_expr | mul_div_expr - add_sub_expr
mul_div_expr -> int_literal | identifier | 
                int_literal * mul_div_expr | identifier * mul_div_expr | 
                ( add_sub_expr )

*)

type expression = 
  | AddExpression of expression * expression 
  | SubExpression of expression * expression
  | MulExpression of expression * expression 
  | DivExpression of expression * expression 
  | Identifier of string 
  | IntLiteral of int
  | FloatLiteral of float

type lvalue = string
type rvalue = expression

type statement = 
  | CompoundStatement of statement list
  | AssignmentStatement of lvalue * rvalue

type typ = 
  | Integer
  | Float 
  | Double 
  | Character 
  | Void

type argument = typ * string

type func = typ * string * argument list * statement

type file = func list

exception ParseError

(* have the parse functions return the remaining tokens, and the part of the tree 
   they have parsed till now 
*)

let parse_typ = function 
  | INT::l    -> (Integer  , l)
  | FLOAT::l  -> (Float    , l)
  | CHAR::l   -> (Character, l)
  | DOUBLE::l -> (Double   , l)
  | VOID::l   -> (Void     , l)
  | _ -> raise ParseError

let parse_identifier = function 
  | (IDENTIFIER id)::l -> (id, l)
  | _ -> raise ParseError

let parse_arg toks = 
  let (typ, rem)  = parse_typ toks in 
  let (name, rem) = parse_identifier rem in 
  ((typ, name), rem)

let rec parse_arg_list = function 
  | LPAREN::l -> (parse_arg_list l)
  | toks -> (
          let (arg, rem) = (parse_arg toks) in 
          match rem with 
          | (COMMA::rlist) -> (let (arg_list, rem) = (parse_arg_list rlist) in (arg::arg_list, rem))
          | (RPAREN::rlist2) -> (arg::[], rlist2)
          | _ -> raise ParseError
    )

let parse_func toks = 
  let ((ret_type, fn_name), rem) = parse_arg toks in 
  let (args, rem) = parse_arg_list rem in 
  ((ret_type, fn_name, args, CompoundStatement []), rem)

let rec parse_file = function 
  | [] -> []
  | toks -> let (func, rem_toks) = (parse_func toks) in func::(parse_file rem_toks)

(*

START -> TU 
TU -> ED | TU ED 
ED -> FN_DEF | DECL 
FN_DEF -> DECL_SPEC DECL DECL_LIST CS | DECL_SPEC DECL CS

DECL -> PTR DIR_DECL | DIR_DECL

DIR_DECL -> 

DECL_LIST -> DECL | DECL_LIST DECL

DECL_SPEC ->   SC_SPEC DECL_SPEC | SC_SPEC 
             | T_SPEC DECL_SPEC | T_SPEC 
             | T_QUAL DECL_SPEC | T_QUAL 
             | FN_SPEC DECL_SPEC | FN_SPEC 
             | ALIGN_SPEC DECL_SPEC | ALIGN_SPEC

Storage classes: https://en.cppreference.com/w/c/language/storage_duration
SC_SPEC -> typedef | extern | static | _Thread_local | auto | register

Types:
T_SPEC  -> void | char | short | int | long | float | double | signed | unsigned | bool | _Complex | _Imaginary | ...

Functions:
FN_SPEC -> inline | _Noreturn



DECLR -> DECL_SPEC ; | DECL_SPEC DECL_LIST ; | STATIC_ASSERT_DECL
DECL_LIST -> INIT_DECL | DECL_LIST , INIT_DECL
INIT_DECL -> DECL = INIT | DECL 
INIT -> { INIT_LIST } | { INIT_LIST , } | ASS_EXPR
INIT_LIST -> DESG INIT | INIT | 
DESG -> DESG_LIST =
DESG_LIST -> DESGNTR | DESG_LIST DESGNTR
DESGNTR -> [ CONST ] | . ID 

// TODO INIT

CS -> {} | { CL }
CL -> ITEM | CL ITEM

ITEM -> DECLR | S 
S -> LS | CS | ES | SS | IS | JS

LS -> ID : S | case CONSTEXPR : S | default : S 
ES -> ; | EXPR ;
SS -> if ( EXPR ) S else S | if ( EXPR ) S | SWITCH ( expr ) S 
IS -> while ( EXPR ) S | do S while ( EXPR ) ; 
      | for ( ES ES ) S | for (ES ES E) S | for ( DECLR ES ) S | for ( DECLR ES E ) S 
JS -> goto ID ; | continue ; | break ; | return ; | return EXPR ;

*)
