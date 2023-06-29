module Lexer

open System.Text.RegularExpressions
open AbSyn


type Token =
    | LET
    | VAR of string
    | EQUAL
    | NUM of int

let tokenize (input: string) =
    let keywords = ["let"; "if"]
    let regexVar = Regex(@"[a-zA-Z]+")
    let regexNum = Regex(@"[0-9]+")

    let rec tokenizeHelper (input: string list) (acc: Token list) =
        match input with
        | [] -> acc
        | "let" :: rest -> tokenizeHelper rest (LET :: acc)
        | "=" :: rest -> tokenizeHelper rest (EQUAL :: acc)
        | token :: rest ->
            if regexVar.IsMatch(token) then
                let varToken = VAR token
                tokenizeHelper rest (varToken :: acc)
            elif regexNum.IsMatch(token) then
                let numToken = NUM (int token)
                tokenizeHelper rest (numToken :: acc)
            else
                failwithf "Invalid token: %s" token

    let matches = Regex.Matches(input, @"\w+")
    let tokens = matches |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toList
    tokenizeHelper tokens [] |> List.rev

let parse (tokens: Token list) =
    match tokens with
    | LET :: VAR varName :: NUM num :: [] ->  Let (Var varName, Num num)
    | _ -> failwith "Invalid syntax"