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
    | NUM of int * Position
    | VAR of string * Position
    | OPERATE of Binop * Position
    | LET of Position
    | IF of Position
    | WHILE of Position
    | EQUAL of Position
    | THEN of Position
    | ELSE of Position
    | LPAREN of Position
    | RPAREN of Position
    | BEGIN of Position
    | END of Position

let tokenize (input: string) =
    let ignoredKeywords = []
    let keywords = ["let"; "if"; "while"; "then"; "else"]
    let regexVar = Regex(@"[a-zA-Z]+")
    let regexNum = Regex(@"[0-9]+")
    let regexBinOp = Regex(@"[+\-*/<>=]")
    let regexParen = Regex(@"[(|)]")
    let regexBraces = Regex(@"[{|}]")  // New regex for braces

    let rec tokenizeHelper (input: string list) (acc: Token list) (line: int) (column: int) =        
        match input with
        | [] -> acc
        | "let" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 2 }
            let newPosition = { startPosition with endColumn = column + 4 }
            let letToken = LET newPosition
            tokenizeHelper rest (letToken :: acc) line (column + 5)
        | "if" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 1 }
            let newPosition = { startPosition with endColumn = column + 2 }
            let ifToken = IF newPosition
            tokenizeHelper rest (ifToken :: acc) line (column + 3)
        | "while" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 4 }
            let newPosition = { startPosition with endColumn = column + 6 }
            let whileToken = WHILE newPosition
            tokenizeHelper rest (whileToken :: acc) line (column + 7)
        | "then" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 3 }
            let newPosition = { startPosition with endColumn = column + 4 }
            let thenToken = THEN newPosition
            tokenizeHelper rest (thenToken :: acc) line (column + 5)
        | "else" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 3 }
            let newPosition = { startPosition with endColumn = column + 4 }
            let elseToken = ELSE newPosition
            tokenizeHelper rest (elseToken :: acc) line (column + 5)
        | "=" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column}
            let newPosition = { startPosition with endColumn = column + 1}
            let equalToken = EQUAL newPosition
            tokenizeHelper rest (equalToken :: acc) line (column + 2)
        | token :: rest when ignoredKeywords |> List.contains token ->  // Skip ignored keywords
            tokenizeHelper rest acc line (column + String.length token + 1)
        | token :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + String.length token + 1}
            let newPosition = { startPosition with endColumn = column + String.length token}
            if regexVar.IsMatch(token) then
                let varToken = VAR (token, newPosition)
                tokenizeHelper rest (varToken :: acc) line (column + String.length token + 1)
            elif regexParen.IsMatch(token) then
                let parenToken = if token = "(" then LPAREN newPosition else RPAREN newPosition 
                tokenizeHelper rest (parenToken :: acc) line (column + String.length token + 1)
            elif regexBraces.IsMatch(token) then  // New case for braces
                let braceToken = if token = "{" then BEGIN newPosition else END newPosition 
                tokenizeHelper rest (braceToken :: acc) line (column + String.length token + 1)
            elif regexNum.IsMatch(token) then
                let numToken = NUM (int token, newPosition)
                tokenizeHelper rest (numToken :: acc) line (column + String.length token)
            elif regexBinOp.IsMatch(token) then
                let binOp = match token with
                            | "+" -> Plus
                            | "-" -> Minus
                            | "*" -> Times
                            | "/" -> Divide
                            | "<" -> Less
                            | ">" -> Greater
                            | "==" -> Equal
                            | _ -> failwithf "Invalid binary operator: %s" token
                let binOpToken = OPERATE (binOp, newPosition)
                tokenizeHelper rest (binOpToken :: acc) line (column + String.length token + 1)
            else
                failwithf "Invalid token: %s" token

    let matches = Regex.Matches(input, @"\b\w+\b|[=+\-*/<>(){}]+")  // Include braces in your regex
    let tokens = matches |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toList
    tokenizeHelper tokens [] 0 0 |> List.rev


let isExprToken token =
    match token with
    | NUM _ | VAR _ | OPERATE _ -> true
    | _ -> false

let isStmtStartToken token =
    match token with
    | LET _ | IF _ | WHILE _ -> true
    | _ -> false

let parse (tokens: Token list) =
    let rec parseExpr tokens =
        match tokens with
        | NUM (n, _) :: rest ->
            (Num n, rest)
        | VAR (v, _) :: rest ->
            (Var v, rest)
        | OPERATE (op, _) :: rest when rest |> List.head |> isExprToken ->
            let (expr1, rest) = parseExpr rest
            let (expr2, rest) = parseExpr rest
            (Operate (op, expr1, expr2), rest)
        | LPAREN pos :: rest ->  // New case for left parenthesis
            let (expr, rest) = parseBinOp rest  // Call to parseBinOp
            printfn "%A" rest
            match rest with
            | RPAREN _ :: rest -> (ParenExpr expr, rest)
            | _ -> failwith "Expected right parenthesis"

        | _ -> failwith "Unexpected token when parsing expression"

    and parseBinOp tokens =
        match tokens with
        | VAR (v, _) :: OPERATE (op, _) :: rest when rest |> List.head |> isExprToken ->
            let (expr, rest) = parseExpr rest
            (Operate (op, Var v, expr), rest)
        | _ -> failwith "Unexpected token when parsing binary operation"


    let rec parseStmt tokens =
        match tokens with
        | LET pos :: VAR (v, _) :: EQUAL _ :: rest ->
            let (expr, rest) = parseExpr rest
            (Let (Var v, expr), rest)
        | IF pos :: rest ->
            let (cond, rest) = parseExpr rest
            let thenStmt, rest = match rest with
                                 | THEN _ :: rest -> parseStmt rest
                                 | _ -> failwith "Expected THEN token"
            let elseStmt, rest = match rest with
                                 | ELSE _ :: rest -> parseStmt rest
                                 | _ -> failwith "Expected ELSE token"
            (If (cond, thenStmt, elseStmt), rest)
        | WHILE pos :: rest ->
            let (cond, rest) = parseExpr rest
            let (bodyStmt, rest) = parseStmt rest
            (While (cond, bodyStmt), rest)
        | _ -> failwith "Unexpected token when parsing statement"

    fst (parseStmt tokens)
    


    
(* and parseExpression (exprTokens: Token list) =
    match exprTokens with
    | [NUM (num, _)] -> Num num
    | [VAR (varName, _)] -> Var varName
    | VAR (varName, _) :: OPERATE (op, _) :: restTokens ->
        let rightExpr = parseExpression restTokens
        Operate (op, Var varName, rightExpr)
    | LET pos :: VAR (varName, pos1) :: EQUAL(pos2) :: restTokens ->
        let rightExpr = parseExpression restTokens
        Let (Var varName, rightExpr)
    | _ -> failwith "Invalid expression in parse" *)

