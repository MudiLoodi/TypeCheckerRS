module AbSyn

type Binop = Plus | Minus | Times | Divide | Less | Greater | Equal

type Position = {
    startLine: int
    startColumn: int
    endLine: int
    endColumn: int
}

type Exp = 
    | Num of int
    | Var of string
    | Operate of Binop * Exp * Exp
    | Let of Exp * Exp 
    | If of Exp * Exp * Exp 
    | Fun of Exp * Exp
    | App of Exp * Exp
    | While of Exp * Exp
    | Record of string * (string * Exp) list // Nested record type to hold mulitple fields
    | RecDot of Exp * string
    | ParenExpr of Exp

type Type = 
    | Low
    | High
    | OK
    | Arr of Type * Type // t1 -> t2
    | Rec of (string * Type) list // fields are Exp (Var)