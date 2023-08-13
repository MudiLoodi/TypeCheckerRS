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
    for e in program do
        let inferredType = findtype finalTenv e 
        // printfn "%A: %A" e inferredType
        let typeCheckResult = hastype finalTenv e inferredType
        match typeCheckResult with 
        | true  -> writer.WriteLine("PASS")
        | _ -> writer.WriteLine("FAIL")
    writer.Close()
run program