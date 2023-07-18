module main

open System.IO
open TypeEnv
open AbSyn
open Lexer
open TypeCheck

exception MyError of string

let readAndParseLinesFromFile (filePath: string) =
    let lines = new ResizeArray<Exp>()
    use fileStream = new StreamReader(filePath)
    while not fileStream.EndOfStream do
        let line = fileStream.ReadLine()
        let tokens = tokenize line
        let exp = parse tokens
        lines.Add(exp)
    List.ofSeq lines

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
    | Let (e1, e2) -> 
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match(e1_type, e2_type) with 
            | (High, _)     -> High
            | (Low, Low)    -> Low
            | (Low, OK)     -> Low
            | (Low, High)   ->  raise (MyError ($"Illegal explicit flow to %A{e1}"))
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
// let filePath = "p"
// let program = readAndParseLinesFromFile filePath
let exp = Let (Var("H_a"), Var("x"))
let program = [exp]

let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv


for e in program do
    printfn "%A" (hastype finalTenv e )