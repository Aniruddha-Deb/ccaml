%{
  open Ast
%}

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMICOLON
%token EQUAL
%token PLUS
%token TYPE_INT
%token TYPE_VOID
%token <int> INT
%token <string> ID
%token EOF

%start main

%type <Ast.main> main
%type <Ast.statement> stmt
%type <Ast.rvalue> rval

%%

main: 
TYPE_VOID; ID; LPAREN; RPAREN; LBRACE; stmts=list(stmt); RBRACE; EOF {Prog(stmts)};

stmt:
    TYPE_INT; v=ID; EQUAL; rvalue=rval; SEMICOLON {Assignment(Variable(Integer, v), rvalue)};

rval:
| a=INT; PLUS; b=rval { BinOp(Addition,Integer(a),b) }
| a=INT {Integer(a)};
