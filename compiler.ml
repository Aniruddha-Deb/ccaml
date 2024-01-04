open Lexing
open Printf
open Ast

exception Error of string

let rec pprint_rval = function 
    | Integer(a) -> sprintf "%d" a
    | BinOp(_, a, b) -> sprintf "(%s + %s)" (pprint_rval a) (pprint_rval b)

let pprint_stmt = function 
    | Assignment(Variable(_, name), rvalue) -> 
    printf "int %s : %s \n" name (pprint_rval rvalue)

let pprint_ast = function 
    | Prog(list) -> printf "main\n"; List.iter pprint_stmt list

let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
    try Parser.main Lexer.toy_lang lexbuf with
  | Parser.Error ->
      let error_msg = sprintf "%s: syntax error" (print_error_position lexbuf) in
      raise (Error error_msg)

let main () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  let result = parse_program lexbuf in 
  pprint_ast result

let () = main ()
