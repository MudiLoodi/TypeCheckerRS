﻿module main

open System.IO
open TypeEnv
open Lexer
open TypeCheck
open AbSyn

exception MyError of string

let customErrorMessage (errorType: string) (expr: Exp) =
    match expr with
    | Let (Var var1, Var var2) -> sprintf "%s in 'let %s = %s'" errorType var1 var2
    | _ -> sprintf "%s" errorType 

let readAndParseLinesFromFile (filePath: string) =
    let lines = new ResizeArray<Exp>()
    use fileStream = new StreamReader(filePath)
    while not fileStream.EndOfStream do
        let line = fileStream.ReadLine()
        let tokens = tokenize line
        printfn "%A" tokens
        let exp = parse tokens
        lines.Add(exp)
    List.ofSeq lines  // Convert lines to a list

// ---------- TYPE INFERENCE ---------- //
let rec findtype tenv exp =
    match exp with
    | Num n -> OK
    | Var(e1) -> 
        let res = lookup e1 tenv
        match res with
            | High  -> High
            | Low   -> Low
    | Operate (op, e1, e2) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with
            | (High, _) | (_, High) -> High
            | (Low, Low)            -> Low
            | (Low, OK)             -> Low
            | (OK, OK)              -> OK
    | Let (e1, e2) -> 
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match(e1_type, e2_type) with 
            | (High, _) -> High
            | (Low, _) ->  Low
            | _ -> Low
    | If (e1, e2, e3) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        let e3_type = findtype tenv e3
        match (e1_type, e2_type, e3_type) with 
            | (High, High, High) -> High
            | (High, Low, Low) -> raise (MyError($"Illegal Implicit Flow from then-branch: %A{e2_type}, else-branch: %A{e3_type} to clause: %A{e1_type}"))
            | (Low, _, _) -> Low
    | Fun (e1, e2) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with 
            | (High, High)    -> Arr (High, High)
            | (Low, t)  -> Arr (Low, t)
            | (High, Low)   -> raise (MyError($"Illegal Implicit Flow in function: %A{e1_type}->%A{e2_type}"))
    | App (e1, e2) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with 
            | (Arr(t1, t2), t1') -> if t1 = t1' then t2 else raise (MyError("Invalid Types."))
            | _ -> raise(MyError("App placeholder err"))
    | While (e1, e2) -> 
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with 
        | (High, High) -> OK
        | (Low, _) -> OK
        | _ -> raise(MyError("While placeholder err"))
    | RecDot (e1, f) -> 
        let res = lookup f tenv
        match res with
            | High  -> High
            | Low   -> Low
            | OK -> OK
            | Arr (t1, t2) -> Arr (t1, t2)
    | ParenExpr (e1) -> 
        let e1_type = findtype tenv e1
        match e1_type with 
            | High -> High
            | Low -> Low
            | _ -> OK
            
(* let exp = Let (Var "y", Num 10)
let exp1 = Let (Var "a", Num 5)
let exp2 = Let (Var "b", Num 6)

let cond = Operate (Greater, Var "y", Num 0)

let ifexp = If (cond, exp, exp1)

let f = Fun(Var "H_x", ifexp) *)

(* let exp1 = Let (Var "H_a", Num 5)
let exp2 = Let (Var "b", Num 6)

let cond = Operate (Greater, Var "H_y", Num 0)

let ifexp = If (cond, exp1, exp2) *)
let filePath = "p"
let program = readAndParseLinesFromFile filePath
(* let exp1 = Let (Var "H_a", Num 5)
let exp2 = Let (Var "H_b", Num 6)
let cond = Operate (Greater, Var "y", Num 0)
let ifexp = If (cond, exp1, exp2)
let f = Fun(Var "H_x", ifexp) // High -> Low *)

(* let r = Record [("CPR", Var "a"); ("ASD", Num 12)]
let dot = RecDot (r, "CPR")
let v = Let (Var "H_a", dot) *)

// let program = [ex]
let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv

let run program =
    for e in program do
        let inferredType = findtype finalTenv e 
        match hastype finalTenv e inferredType with 
        | true -> true
        | false ->  
            failwith  (customErrorMessage "Illegal explicit flow" e)
        |> ignore
printfn "%A" (run program)