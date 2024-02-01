open Re

type token = AUTO
  | BREAK
  | CASE
  | CHAR
  | CONST
  | CONTINUE
  | DEFAULT
  | DO
  | DOUBLE
  | ELSE
  | ENUM
  | EXTERN
  | FLOAT
  | FOR
  | GOTO
  | IF
  | INLINE
  | INT
  | LONG
  | REGISTER
  | RESTRICT
  | RETURN
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | STRUCT
  | SWITCH
  | TYPEDEF
  | UNION
  | UNSIGNED
  | VOID
  | VOLATILE
  | WHILE
  | ALIGNAS
  | ALIGNOF
  | ATOMIC
  | BOOL
  | COMPLEX
  | GENERIC
  | IMAGINARY
  | NORETURN
  | STATIC_ASSERT
  | THREAD_LOCAL
  | FUNC_NAME
  | ELLIPSIS
  | RIGHT_ASSIGN
  | LEFT_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | OR_ASSIGN
  | RIGHT_OP
  | LEFT_OP
  | INC_OP
  | DEC_OP
  | PTR_OP
  | AND_OP
  | OR_OP
  | LE_OP
  | GE_OP
  | EQ_OP
  | NE_OP
  | SEMICOLON
  | LBRACE
  | RBRACE
  | PERIOD
  | LPAREN
  | RPAREN
  | COMMA
  | COLON
  | EQUALTO
  | LBOX
  | RBOX
  | AMPERSAND
  | EXCLMARK
  | TILDE
  | MINUS
  | PLUS
  | STAR
  | SLASH
  | PERCENT
  | LESSTHAN
  | GREATERTHAN
  | CARET
  | OR
  | QUESMARK
  | INT_LITERAL of string 
  | FLOAT_LITERAL of string 
  | STRING_LITERAL of string
  | IDENTIFIER of string 
  | ERROR
  | WHITESPACE
  | COMMENT

(* taken from https://www.quut.com/c/ANSI-C-grammar-y-2011.html *)
let t_O   = (rg '0' '7')
let t_D   = (rg '0' '9')
let t_NZ  = (rg '1' '9')
let t_LC  = (rg 'a' 'z')
let t_UC  = (rg 'A' 'Z')
let t_L   = alt [t_LC; t_UC; char '_']
let t_A   = alt [t_LC; t_UC; char '_'; t_D]
let t_H   = alt [rg 'a' 'f'; rg 'A' 'F'; t_D]
let t_HP  = seq [char '0'; alt [char 'x'; char 'X']]
let t_E   = seq [alt [char 'e'; char 'E']; opt (alt [char '-'; char '+']); rep1 t_D]
let t_P   = seq [alt [char 'p'; char 'P']; opt (alt [char '-'; char '+']); rep1 t_D]
let t_FS  = alt [char 'f'; char 'F'; char 'l'; char 'L']
let t_IS  = alt [str "ul";  str "Ul";  str "uL";  str "UL";  str "lu";  str "Lu"; 
                 str "lU";  str "LU";  str "llu"; str "llU"; str "LLu"; str "LLU"; 
                 str "ulu"; str "Ulu"; str "uLu"; str "ULu"]
let t_CP = alt [char 'u'; char 'U'; char 'L']
let t_SP = alt [str "u8"; char 'u'; char 'U'; char 'L']
let t_ES = seq [char '\\'; alt [
        alt [char '\''; char '"'; char '?'; char '\\'; char 'a'; char 'b'; char 'f'; char 'n'; char 'r'; char 't'; char 'v'];
        repn t_O 1 (Some 3);
        seq [char 'x'; rep1 t_H]
    ]]

let t_WS = space

let re_int = compile (seq [start; alt [ 
                seq [t_HP; (rep1 t_H); opt t_IS];
                seq [t_NZ; (rep t_D);  opt t_IS];
                seq [char '0'; (rep t_O);  opt t_IS]
            ]])
let re_float = compile (seq [start; alt [
                seq [ rep1 t_D; t_E; opt t_FS];
                seq [ rep t_D; char '.'; rep1 t_D; opt t_E; opt t_FS];
                seq [ rep1 t_D; char '.'; opt t_E; opt t_FS];
                seq [ t_HP; rep1 t_H; t_P; opt t_FS];
                seq [ t_HP; rep t_H; char '.'; rep1 t_H; t_P; opt t_FS];
                seq [ t_HP; rep1 t_H; char '.'; t_P; opt t_FS]
            ]])

let re_str = compile (whole_string (seq [start;
    rep1 (seq [
        opt t_SP; char '"';
        rep (alt [
            (diff notnl (alt [char '"'; char '\\']));
            t_ES 
        ]);
        char '"'; (rep t_WS)
        ])
    ]))

let re_identifier = compile (seq [start; t_L; rep t_A])

let re_comment = compile (seq [start; alt [seq [str "//"; rep notnl]; seq [str "/*"; rep any; str "*/"]]])

let re_whitespace = compile (seq [start; rep1 t_WS])

let str2tok_re s = 
    if      (execp re_whitespace s) then WHITESPACE
    else if (execp re_identifier s) then IDENTIFIER s
    else if (execp re_int        s) then INT_LITERAL s
    else if (execp re_float      s) then FLOAT_LITERAL s
    else if (execp re_str        s) then STRING_LITERAL s
    else ERROR

let str2tok = function
  | "auto"      -> AUTO
  | "break"     -> BREAK
  | "case"      -> CASE
  | "char"      -> CHAR
  | "const"     -> CONST
  | "continue"  -> CONTINUE
  | "default"   -> DEFAULT
  | "do"        -> DO
  | "double"    -> DOUBLE
  | "else"      -> ELSE
  | "enum"      -> ENUM
  | "extern"    -> EXTERN
  | "float"     -> FLOAT
  | "for"       -> FOR
  | "goto"      -> GOTO
  | "if"        -> IF
  | "inline"    -> INLINE
  | "int"       -> INT
  | "long"      -> LONG
  | "register"  -> REGISTER
  | "restrict"  -> RESTRICT
  | "return"    -> RETURN
  | "short"     -> SHORT
  | "signed"    -> SIGNED
  | "sizeof"    -> SIZEOF
  | "static"    -> STATIC
  | "struct"    -> STRUCT
  | "switch"    -> SWITCH
  | "typedef"   -> TYPEDEF
  | "union"     -> UNION
  | "unsigned"  -> UNSIGNED
  | "void"      -> VOID
  | "volatile"  -> VOLATILE
  | "while"     -> WHILE
  (* C11 stuff? *)
  | "_Alignas"       -> ALIGNAS
  | "_Alignof"       -> ALIGNOF
  | "_Atomic"        -> ATOMIC
  | "_Bool"          -> BOOL
  | "_Complex"       -> COMPLEX
  | "_Generic"       -> GENERIC
  | "_Imaginary"     -> IMAGINARY
  | "_Noreturn"      -> NORETURN
  | "_Static_assert" -> STATIC_ASSERT
  | "_Thread_local"  -> THREAD_LOCAL
  | "__func__"       -> FUNC_NAME
  | "..."       -> ELLIPSIS
  | ">>="       -> RIGHT_ASSIGN
  | "<<="       -> LEFT_ASSIGN
  | "+="        -> ADD_ASSIGN
  | "-="        -> SUB_ASSIGN
  | "*="        -> MUL_ASSIGN
  | "/="        -> DIV_ASSIGN
  | "%="        -> MOD_ASSIGN
  | "&="        -> AND_ASSIGN
  | "^="        -> XOR_ASSIGN
  | "|="        -> OR_ASSIGN
  | ">>"        -> RIGHT_OP
  | "<<"        -> LEFT_OP
  | "++"        -> INC_OP
  | "--"        -> DEC_OP
  | "->"        -> PTR_OP
  | "&&"        -> AND_OP
  | "||"        -> OR_OP
  | "<="        -> LE_OP
  | ">="        -> GE_OP
  | "=="        -> EQ_OP
  | "!="        -> NE_OP
  | ";"         -> SEMICOLON
  | "{"         -> LBRACE
  | "<%"        -> LBRACE
  | "}"         -> RBRACE
  | "%>"        -> RBRACE
  | "."         -> PERIOD
  | "("         -> LPAREN
  | ")"         -> RPAREN
  | ","         -> COMMA
  | ":"         -> COLON
  | "="         -> EQUALTO
  | "["         -> LBOX
  | "]"         -> RBOX
  | "<:"        -> LBOX
  | ":>"        -> RBOX
  | "&"         -> AMPERSAND
  | "!"         -> EXCLMARK
  | "~"         -> TILDE
  | "-"         -> MINUS
  | "+"         -> PLUS
  | "*"         -> STAR
  | "/"         -> SLASH
  | "%"         -> PERCENT
  | "<"         -> LESSTHAN
  | ">"         -> GREATERTHAN
  | "^"         -> CARET
  | "|"         -> OR
  | "?"         -> QUESMARK
  | _           -> ERROR

(* TODO create string_to_int and string_to_float methods to actually parse 
 and convert the int/float literals to OCaml ints/floats *)

let re_munch s = 
    match matches re_whitespace s with 
    | m::_2 -> Some (WHITESPACE, String.length m)
    | [] ->
    match matches re_comment s with 
    | m::_2 -> Some (COMMENT, String.length m)
    | [] ->
    match matches re_str s with 
    | m::_ -> Some (STRING_LITERAL m, String.length m)
    | [] -> 
    match matches re_float s with 
    | m::_ -> Some (FLOAT_LITERAL m, String.length m)
    | [] -> 
    match matches re_int s with
    | m::_ -> Some (INT_LITERAL m, String.length m)
    | [] -> 
    match matches re_identifier s with
    | m::_ -> if (str2tok m) = ERROR then Some (IDENTIFIER m, String.length m) else None
    | [] -> 
    None

let rec munch s = function 
    | 0 -> (match re_munch s with
            | Some t -> t
            | None -> (munch s 1))
    | i -> 
    let open String in
    let prefix = (sub s 0 i) in
    let ptok = (str2tok prefix) in
    if (i+1) <= (length s) then
        if ptok = ERROR then
            munch s (i+1)
        else 
            let p2 = (sub s 0 (i+1)) in 
            let ptok2 = (str2tok p2) in
            if (ptok != ERROR) && (ptok2 != ERROR) then
            munch s (i+1)
        else
            (ptok, i)
    else
        (ptok, i)

let discard = function
    | WHITESPACE -> true 
    | COMMENT -> true 
    | _ -> false

let rec tokenize = function 
    | "" -> []
    | span -> let (token, toklen) = munch span 0 in 
              let suffix = (String.sub span toklen ((String.length span)-toklen)) in
              if (discard token) then tokenize suffix
              else token::(tokenize suffix)
