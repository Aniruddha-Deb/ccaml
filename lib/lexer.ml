open Re

type token = 
    LPAREN | 
    RPAREN |
    LBRACE |
    RBRACE |
    OP_EQUAL |
    OP_PLUS |
    INT |
    VOID |
    EOF |
    IDENTIFIER of string |
    INTEGER of int

let re_keyword = compile (whole_string (seq [str "int"; str "void"]))
let re_identifier = compile (whole_string (seq [alt [rg 'a' 'z'; rg 'A' 'Z']; rep (alt [rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9'])]))
let re_integer = compile (whole_string (seq [bos; opt (char '-'); rep1 (rg '0' '9')]))
let re_operator = compile ((seq [bos; alt [char '='; char '+'; char '-'; char '*']]))
let re_comment = compile (whole_string (seq [bos; str "//"; rep print]))
let re_whitespace = compile (rep space)

let split_ws str = Re.split re_whitespace str

(*

Token classes:
- Keywords 
- Identifiers 
- Operators 
- Literals
- Comments

Keywords -> lookahead needed to differentiate 
Identifiers -> lookahead 
Operators -> 

Lexing by hand:
while the string is not exhausted:
- Accumulate characters in temp
- Check if any more characters left in span
  - If yes, peek ahead and try matching the new accumulated string with a token
  - If it matches, continue accumulating (maximal munch)
  - If not, match the accumulated token with available tokens

eg intmax - has int as substr but not parsed as [int max]!
- maximal munch for identifiers. 
*)

let string_to_token = function 
    | "int" -> INT 
    | "void" -> VOID 
    | "(" -> LPAREN 
    | ")" -> RPAREN
    | "{" -> LBRACE 
    | "}" -> RBRACE 
    | "=" -> OP_EQUAL 
    | "+" -> OP_PLUS 
    | s -> if Re.execp re_integer s then INTEGER (int_of_string s) else IDENTIFIER s

let token_matches token = 
    Re.execp re_keyword token || Re.execp re_identifier token || 
    Re.execp re_integer token || Re.execp re_operator token

let rec maximal_munch span idx = 
    if String.length span >= (idx+1) then
        if not (token_matches (String.sub span 0 idx)) then
            maximal_munch span idx+1
        else if (token_matches (String.sub span 0 idx)) && (token_matches (String.sub span 0 (idx+1))) then
            maximal_munch span idx+1
        else
            idx
    else
        idx

let split_string_at s idx = ((String.sub s 0 idx), (String.sub s idx ((String.length s)-idx)))

let rec match_span = function 
    | "" -> []
    | span -> let token_len = maximal_munch span 1 in 
              let (prefix, suffix) = split_string_at span token_len in
              (string_to_token prefix)::(match_span suffix)

let rec tokenize = function 
    | span::s -> List.concat [(match_span span); (tokenize s)]
    | [] -> []

let tokenize_str s = tokenize (split_ws s)
