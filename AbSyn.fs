module AbSyn

type Binop = Plus | Minus | Times | Divide | Less | Greater | Equal

type Value =
    | IntVal of int
    | StrVal of string

type Exp = 
    | Num of int
    | Var of string
    | Operate of Binop * Exp * Exp
    | Let of Exp * Exp
    | If of Exp * Exp * Exp 

type Types = 
    | High
    | Low
    | OK