module TypeCheck

open System.IO
open TypeEnv
open AbSyn
open Lexer

exception MyError of string

let rec hastype tenv exp expectedType =
    match exp with 
    | Num n -> 
        match expectedType with
        | Low -> true
        | _ -> false
    | Var(e1) -> 
        let foundtype = lookup e1 tenv 
        match foundtype, expectedType with
            | t1, t2 -> t1 = t2

    | Operate (op, e1, e2) -> 
        match expectedType with
        | High -> 
            let t1 = hastype tenv e1 High 
            let t2 = hastype tenv e2 High 
            t1 || t2
        | Low -> 
            let t1 = hastype tenv e1 Low 
            let t2 = hastype tenv e2 Low
            t1 && t2
        | _ -> false

    | Let (e1, e2) -> 
        match expectedType with 
        | High -> 
            let t1 = hastype tenv e1 High 
            if t1 then t1 else raise (MyError ("High err"))
        | Low -> 
            let t1 = hastype tenv e1 High 
            let t2 = hastype tenv e2 Low
            let res = not t1 && t2
            //if res then res else raise (MyError ("Illegal explicit flow at " + $"%A{e1}")) 
            res
        | _ -> raise (MyError ("asd"))

    | If (e1, e2, e3) ->
        match expectedType with 
        | High -> 
            let t1 = hastype tenv e1 High 
            let t2 = hastype tenv e2 High 
            let t3 = hastype tenv e3 High 
            t1 && t2 && t3
        | Low -> 
            let t1 = hastype tenv e1 Low 
            t1 
        | _ -> false

    | App (e1, e2) ->
        match expectedType with 
        | High -> // if t2 is high, then e1 can either be high -> high or low -> high
            let t1 = hastype tenv e1 (Arr (High, High)) || hastype tenv e1 (Arr (Low, High))
            let t2 = hastype tenv e2 High
            t1 && t2
        | Low -> 
            let t1 = hastype tenv e1 (Arr (Low, Low)) || hastype tenv e1 (Arr (High, Low))
            let t2 = hastype tenv e2 Low
            t1 && t2
        | _ -> false

    | Fun (e1, e2) ->
        match expectedType with 
        | Arr (expt1, expt2) -> 
            let t1 = hastype tenv e1 expt1 
            let t2 = hastype tenv e2 expt2
            t1 && t2
        | _ -> false

    | While (e1, e2) -> 
        match expectedType with 
        | OK -> 
            let t1 = hastype tenv e1 Low 
            if t1 then true 
            elif not t1 then 
                let t2 = hastype tenv e2 High
                not t1 && t2
            else false
        | _ -> false

    | Record (ID, fields) -> 
           true
    | RecDot (e1, f) -> 
        let foundtype = lookup f tenv 
        match foundtype, expectedType with
            | t1, t2 -> t1 = t2
    | ParenExpr (e1) -> 
        match expectedType with 
        | Low -> 
            let t1 = hastype tenv e1 Low 
            t1
        | High -> 
            let t1 = hastype tenv e1 High
            t1
        | _ -> false