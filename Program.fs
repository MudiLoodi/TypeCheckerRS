module main

open System.IO
open TypeEnv
open Lexer
open TypeCheck
open AbSyn
open TypeInference
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

            
(* let bod = Let (Var "x", Num 5)
let e = Fun (Var "a", bod)

let v = Let (Var "g", e) *)
let v = Var "H_a"
let r = Record (["CPR", v; "Age", Num 12])
let dot = RecDot (r, "CPR")

// let filePath = "tests/tests"
// let program = readAndParseLinesFromFile filePath


let exp = Let (Var "y", Num 10)
let exp1 = Let (Var "a", Num 5)
let exp2 = Let (Var "b", Num 6)
let cond = Operate (Greater, Var "t", Var "o")
let ifexp = If (cond, exp1, exp2)
let f = Fun(Var "x", ifexp)

let d = Let (Var "w", Operate (Plus, f, Num 3))
let program = [v;r;dot]

let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv

let run program =
    for e in program do
        let inferredType = findtype finalTenv e 
        let typeCheckResult = hastype finalTenv e inferredType
        //printfn "%A: %A" e inferredType
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