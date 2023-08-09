module main

open System.IO
open TypeEnv
open Lexer
open TypeCheck
open AbSyn
open TypeInference

let readAndParseLinesFromFile (filePath: string) =
    let lines = new ResizeArray<Exp>()
    use fileStream = new StreamReader(filePath)
    while not fileStream.EndOfStream do
        let line = fileStream.ReadLine()
        let tokens = tokenize line
        let exp = parse tokens
        lines.Add(exp)
    List.ofSeq lines  // Convert lines to a list

            
(* let bod = Let (Var "x", Num 5)
let e = Fun (Var "a", bod)

let v = Let (Var "g", e) *)
let v = Var "H_a"
let r = Record ("person", ["CPR", v; "Age", Num 12])
let dot = RecDot (r, "Age")
let op = Operate (Plus, dot, dot)
let p = Let (Var "s", op)
// let filePath = "tests/tests"
// let program = readAndParseLinesFromFile filePath


let exp = Let (Var "y", Num 10)
let exp1 = Let (Var "a", Num 5)
let exp2 = Let (Var "b", Num 6)
let cond = Operate (Greater, Var "t", Var "o")
let ifexp = If (cond, exp1, exp2)
let f = Fun(Var "x", ifexp)

let d = Let (Var "w", Operate (Plus, f, Num 3))
let program = [v;r;dot;p]

let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv

printfn "%A" finalTenv
let run program =
    for e in program do
        let inferredType = findtype finalTenv e 
        let typeCheckResult = hastype finalTenv e inferredType
        // printfn "%A: %A" e inferredType
        match typeCheckResult with 
        | true  -> printfn "PASS"
        | _ -> printfn "FAIL"
(* let run program =
    let filePath = "tests/testResult"
    let writer = File.CreateText(filePath)
    for e in program do
        let inferredType = findtype finalTenv e 
        // printfn "%A: %A" e inferredType
        let typeCheckResult = hastype finalTenv e inferredType
        match typeCheckResult with 
        | true  -> writer.WriteLine("PASS")
        | _ -> writer.WriteLine("FAIL")
    writer.Close() *)
run program