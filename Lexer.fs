module Lexer

open System.Text.RegularExpressions
open AbSyn


type Position = {
    startLine: int
    startColumn: int
    endLine: int
    endColumn: int
}

type Token =
    | LET of Position
    | VAR of string * Position
    | EQUAL of Position
    | NUM of int * Position

let tokenize (input: string) =
    let keywords = ["let"; "if"]
    let regexVar = Regex(@"[a-zA-Z]+")
    let regexNum = Regex(@"[0-9]+")

    let rec tokenizeHelper (input: string list) (acc: Token list) (line: int) (column: int) =        
        match input with
        | [] -> acc
        | "let" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 2 }
            let newPosition = { startPosition with endColumn = column + 4 }
            let letToken = LET newPosition
            tokenizeHelper rest (letToken :: acc) line (column + 5)
        | "=" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column}
            let newPosition = { startPosition with endColumn = column + 1}
            let equalToken = EQUAL newPosition
            tokenizeHelper rest (equalToken :: acc) line (column + 2)
        | token :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + String.length token + 1}
            let newPosition = { startPosition with endColumn = column + String.length token}
            if regexVar.IsMatch(token) then
                let varToken = VAR (token, newPosition)
                tokenizeHelper rest (varToken :: acc) line (column + String.length token + 1)
            elif regexNum.IsMatch(token) then
                let numToken = NUM (int token, newPosition)
                tokenizeHelper rest (numToken :: acc) line (column + String.length token)
            else
                failwithf "Invalid token: %s" token

    let matches = Regex.Matches(input, @"[\w=]+")
    let tokens = matches |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toList
    tokenizeHelper tokens [] 0 0 |> List.rev


let parse (tokens: Token list) =
    match tokens with
    | LET pos :: VAR (varName, pos1) :: EQUAL(pos2) :: NUM (num, pos3) :: [] ->  Let (Var (varName), Num (num))
    | LET pos :: VAR (varName, pos1) :: EQUAL(pos2) :: VAR (varName0, pos3) :: [] -> Let (Var (varName), Var (varName0))
    | _ -> failwith "Invalid syntax"