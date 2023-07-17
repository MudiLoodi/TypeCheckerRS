module TypeCheck

open System.IO
open TypeEnv
open AbSyn
open Lexer
let rec hastype tenv exp typ =
    match exp with 
    | Var(e1) -> 
        match lookup e1 tenv with
            | foundtyp -> foundtyp = typ
    | Operate (op, e1, e2) -> 
        let t1 = hastype tenv e1 High // assume that both exps are of type high
        let t2 = hastype tenv e2 High 
        match typ with
        | High -> t1 || t2 // Only one exp needs to be high 
        | Low -> not t1 && not t2 // Neither of the exp should be high
    | Let (e1, e2) -> 
        let t1 = hastype tenv e1 High // assume that both exps are of type high
        let t2 = hastype tenv e2 High 
        match (t1, t2) with
        | true, _ -> true
        | _, t2 -> not t2