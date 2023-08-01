module TypeCheck
open System
open System.IO
open TypeEnv
open AbSyn
open Lexer

exception TypeError of string 

let getBinopSymbol sym = 
    match sym with 
        | Plus -> "+" 
        | Minus -> "-" 
        | Times -> "*" 
        | Divide -> "/" 
        | Less -> "<" 
        | Greater -> ">" 
        | Equal -> "=="
let rec expressionToString exp = 
    match exp with 
    | ParenExpr e -> expressionToString e
    | Num n -> sprintf "%d" n
    | Operate (op, e1, e2) -> sprintf "%s %s %s" (expressionToString e1) (getBinopSymbol op) (expressionToString e2)
    | Var (v) -> sprintf "%s" v
    | Let (Var v1, e ) -> sprintf "let %s = %s" v1 (expressionToString e) 
    | If (e1, e2, e3) -> sprintf "if (%s) then %s else %s" (expressionToString e1) (expressionToString e2) (expressionToString e3)
    | While (e1, e2) -> sprintf "while (%s) do %s" (expressionToString e1) (expressionToString e2) 
    | Fun (e1, e2) -> sprintf "\\lambda(%s) = {%s}" (expressionToString e1) (expressionToString e2) 
    | App (e1, e2) -> sprintf "%s %s"(expressionToString e1) (expressionToString e2) 
    | RecDot (e1, str) -> sprintf "%s.%s" (expressionToString e1) str
let rec hastype tenv exp inferredType =
    match exp with 
    | Num n -> 
        match inferredType with
        | Low -> true
        | _ -> false
    | Var(e1) -> 
        let foundtype = lookup e1 tenv 
        match foundtype, inferredType with
            | t1, t2 -> t1 = t2

    | Operate (op, e1, e2) -> 
        match inferredType with
        | High -> 
            let t1 = hastype tenv e1 High 
            let t2 = hastype tenv e2 High 
            t1 || t2 // If either expression has type high then it is correct
        | Low -> 
            let t1 = hastype tenv e1 Low 
            let t2 = hastype tenv e2 Low
            t1 && t2
        | _ -> false

    | Let (e1, e2) -> 
        let exp = Let (e1, e2)
        match inferredType with 
        | High -> 
            let t1 = hastype tenv e1 High
            t1
        | Low -> 
            try
                let t1 = hastype tenv e1 High 
                let t2 = hastype tenv e2 Low || hastype tenv e2 OK
                let res = not t1 && t2
                if res then res else raise (TypeError ("Illegal explicit flow at " + $"'{expressionToString exp}'")) 
            with
                | :? TypeError  as ex -> 
                    printfn "%s" ex.Message 
                    false
        | _ -> false
(*         | OK -> 
            let t1 = hastype tenv e1 High
            if t1 then true
            elif not t1 then
                let t2 = hastype tenv e2 Low
                let res = not t1 && t2
                if res then res else raise (TypeError ("Illegal explicit flow at " + $"'{expressionToString exp}'"))
            else
                false
        | _ -> false *)

    | If (e1, e2, e3) ->
        let exp = If (e1, e2, e3)
        match inferredType with 
        | High -> 
            try 
                let t1 = hastype tenv e1 High 
                let t2 = hastype tenv e2 High 
                let t3 = hastype tenv e3 High 
                let res = t1 && t2 && t3
                if res then res else raise (TypeError ("Illegal implicit flow at " + $"'{expressionToString exp}'"))
            with
            | :? TypeError as ex -> 
                printfn "%s" ex.Message
                false
        | Low -> 
            let t1 = hastype tenv e1 Low 
            t1 
        | _ -> false

    | App (e1, e2) ->
        let exp = App (e1, e2)
        match inferredType with 
        | High -> // if t2 is high, then e1 can either be high -> high or low -> high
            let highToHigh = hastype tenv e1 (Arr (High, High)) 
            let lowToHigh =  hastype tenv e1 (Arr (Low, High))
            if highToHigh then 
                let t2 = hastype tenv e2 High
                let res = highToHigh && t2
                if res then res else raise (TypeError ("Illegal types " + $"'{expressionToString e1}' applied to '{expressionToString e2}'"))
            elif lowToHigh then
                let t2 = hastype tenv e2 Low
                let res = lowToHigh && t2
                res
            else
                raise (TypeError ("Illegal types " + $"'{expressionToString e1}' applied to '{expressionToString e2}'"))
        | Low -> 
            let t1 = hastype tenv e1 (Arr (Low, Low)) || hastype tenv e1 (Arr (High, Low))
            let t2 = hastype tenv e2 Low
            let res = t1 && t2
            if res then res else raise (TypeError ("Illegal types " + $"'{expressionToString e1}' applied to '{expressionToString e2}'"))
        | _ -> false

    | Fun (e1, e2) ->
        let exp = Fun (e1, e2)
        match inferredType with 
        | Arr (expt1, expt2) -> 
            try 
                let t1 = hastype tenv e1 High // check if e1 is high
                let expectedType1 = hastype tenv e1 expt1 // check the expected type
                if t1 && expectedType1 then // if e1 and expected type for e1 are High
                    let t2 = hastype tenv e2 Low // Check if e2 is low.
                    let expectedType2 = hastype tenv e2 expt2 
                    let res = not t2 && expectedType2
                    if res then res else raise (TypeError ("Illegal implicit flow in function " + $"'{expressionToString exp}'"))
                else
                    let t1 = hastype tenv e1 Low // check if e1 is low
                    let expectedType1 = hastype tenv e1 expt1 // check the expected type
                    t1 && expectedType1  // if e1 is low, then we are done, since types low -> high and low -> low are accepted.
            with
            | :? TypeError as ex -> 
                printfn "%s" ex.Message
                false
        | _ -> false

    | While (e1, e2) -> 
        match inferredType with 
        | OK -> 
            try 
                let t1 = hastype tenv e1 Low 
                if t1 then true 
                elif not t1 then 
                    let t2 = hastype tenv e2 High
                    let res = not t1 && t2
                    if res then res else raise (TypeError ("Illegal implicit flow at " + $"'{expressionToString exp}'"))
                else false
            with 
            | :? TypeError as ex -> 
                printfn "%s" ex.Message
                false
        | _ -> false

    | Record (fields) -> 
           true
    | RecDot (e1, f) -> 
        let foundtype = lookup f tenv 
        match foundtype, inferredType with
            | t1, t2 -> t1 = t2
    | ParenExpr (e1) -> 
        match inferredType with 
        | Low -> 
            let t1 = hastype tenv e1 Low 
            t1
        | High -> 
            let t1 = hastype tenv e1 High
            t1
        | _ -> false