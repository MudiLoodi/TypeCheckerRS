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



(* let exp1    = Let (Var "H_a", Num 5)
let exp2    = Let (Var "H_b", Num 6)
let cond    = Operate (Greater, Var "H_x", Num 0)
let ifexp   = If (cond, exp1, exp2)
let f       = Fun(Var "H_x", ifexp)
let exp3    = Let (Var "H_y", Num 10)
let app     = App (f, exp3)

let assign = Let (Var "res", app)
let program = [assign] *)

let filePath = "tests/tests"
let program = readAndParseLinesFromFile filePath

let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv

let run program =
    let filePath = "tests/testResult"
    let writer = File.CreateText(filePath)
    use fileStream = new StreamReader("tests/tests")
    //let programContent = fileStream.ReadToEnd() // Read and return the entire file content as a string
    //printfn "\nProgram: \n--------------------------------------------------------------\n%s\n--------------------------------------------------------------" programContent
    for e in program do
        let inferredType = findtype finalTenv e 
        printfn "\n* Expression: %A" e
        printfn "- Inferred Type: %A\n" inferredType
        let typeCheckResult = hastype finalTenv e inferredType
        match typeCheckResult with 
        | true  -> writer.WriteLine("PASS")
        | _ -> writer.WriteLine("FAIL")
    writer.Close()
run program