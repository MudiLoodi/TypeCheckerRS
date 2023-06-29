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
    | Fun of Exp * Exp
    | App of Exp * Exp

type Type = 
    | High
    | Low
    | OK
    | Arr of Type * Type // t1 -> t2