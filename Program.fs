module main

open System.IO
open TypeEnv
open Lexer
open TypeCheck
open AbSyn

(* let rec customErrorMessage (expr: Exp) =
    match expr with
    | Let (Var var1, Var var2 ) -> sprintf "let %s = %s"  var1 var2
    | Let (Var var1, Num (n) ) -> sprintf "let %s = %i"  var1 n
    | If (e1, e2, e3) -> 
        match e1 with
        | ParenExpr (Operate (op, Var v1, Var v2)) -> 
            sprintf "Implicit flow in 'if %s %s %s then %s else %s" v1 (getBinopSymbol op) v2 (customErrorMessage e2) (customErrorMessage e3)
    | _ -> sprintf "placeholder"  *)

let readAndParseLinesFromFile (filePath: string) =
    let lines = new ResizeArray<Exp>()
    use fileStream = new StreamReader(filePath)
    while not fileStream.EndOfStream do
        let line = fileStream.ReadLine()
        let tokens = tokenize line
        let exp = parse tokens
        lines.Add(exp)
    List.ofSeq lines  // Convert lines to a list

// ---------- TYPE INFERENCE ---------- //
let rec findtype tenv exp =
    match exp with
    | Num n -> OK
    | Var(e1) -> 
        let res = lookup e1 tenv
        res
    | Operate (op, e1, e2) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with
            | (High, _) | (_, High) -> High
            | (Low, _)              -> Low
            | (OK, OK)              -> OK
    | Let (e1, e2) -> 
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match(e1_type, e2_type) with 
            | (High, _) -> High
            | (Low, _) ->  Low
            | _ -> OK
    | If (e1, e2, e3) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        let e3_type = findtype tenv e3
        match (e1_type, e2_type, e3_type) with 
            | (High, _, _) -> High
            | (Low, _, _) -> Low
    | Fun (e1, e2) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with 
            | (High, High)    -> Arr (High, High)
            | (High, OK)      -> Arr (High, OK)
            | (Low, t)  -> Arr (Low, t)
            | (High, Low)     -> Arr (High, Low)
    | App (e1, e2) ->
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with 
            | (Arr(t1, t2), t1') -> if t1 = t1' then t2 else t1
    | While (e1, e2) -> 
        let e1_type = findtype tenv e1
        let e2_type = findtype tenv e2
        match (e1_type, e2_type) with 
        | (High, High) -> OK
        | (Low, _) -> OK
        | _ -> OK
    | RecDot (e1, f) -> 
        let res = lookup f tenv
        res
    | ParenExpr (e1) -> 
        let e1_type = findtype tenv e1
        match e1_type with 
            | High -> High
            | Low -> Low
            | _ -> OK
    | Record (e1) -> Low
            
(* let bod = Let (Var "x", Num 5)
let e = Fun (Var "a", bod)

let v = Let (Var "g", e) *)
let r = Record (["CPR", Var "H_a"])
let dot = RecDot (r, "CPR")

let filePath = "tests/tests"
// let program = readAndParseLinesFromFile filePath


let exp = Let (Var "y", Num 10)
let exp1 = Let (Var "a", Num 5)
let exp2 = Let (Var "b", Num 6)
let cond = Operate (Greater, Var "t", Var "o")
let ifexp = If (cond, exp1, exp2)
let f = Fun(Var "x", ifexp)

let d = Let (Var "w", Operate (Plus, f, Num 3))
let program = [f]

let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv

let run program =
    for e in program do
        let inferredType = findtype finalTenv e 
        let typeCheckResult = hastype finalTenv e inferredType
        printfn "%A %A" e inferredType
        match typeCheckResult with 
        | true  -> printfn "PASS"
        | _ -> printfn "FAIL"


(* let run program =
    let filePath = "tests/testResult"
    let writer = File.CreateText(filePath)
    for e in program do
        let inferredType = findtype finalTenv e 
        let typeCheckResult = hastype finalTenv e inferredType
        match typeCheckResult with 
        | true  -> writer.WriteLine("pass")
        | _ -> writer.WriteLine("FAIL")
    writer.Close() *)
run program