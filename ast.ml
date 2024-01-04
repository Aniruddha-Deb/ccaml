type bin_op = Addition

type typ = Integer

type variable = Variable of typ * string

type rvalue = 
    | BinOp of bin_op * rvalue * rvalue
    | Integer of int

type statement = Assignment of variable * rvalue

type main = Prog of statement list 
