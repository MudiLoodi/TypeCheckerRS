module Lexer

open System.Text.RegularExpressions
open AbSyn

type Token =
    | NUM of int * Position
    | VAR of string * Position
    | OPERATE of Binop * Position
    | LET of Position
    | IF of Position
    | WHILE of Position
    | RECORD of Position
    | COMMA of Position
    | COLON of Position
    | EQUAL of Position
    | THEN of Position
    | ELSE of Position
    | LPAREN of Position
    | RPAREN of Position
    | BEGIN of Position
    | END of Position
    | DOT of Position
    | DO of Position

let tokenize (input: string) =
    let ignoredKeywords = []
    let keywords = ["let"; "if"; "while"; "then"; "else"; "do"]
    let regexVar = Regex(@"[a-zA-Z]+")
    let regexNum = Regex(@"[0-9]+")
    let regexBinOp = Regex(@"[+\-*/<>=]")
    let regexParen = Regex(@"[(|)]")
    let regexBraces = Regex(@"[{|}]")
    let regexDot = Regex(@"\.")

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
        | "do" :: rest -> 
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 3 }
            let newPosition = { startPosition with endColumn = column + 4 }
            let elseToken = DO newPosition
            tokenizeHelper rest (elseToken :: acc) line (column + 5)
        | "=" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column}
            let newPosition = { startPosition with endColumn = column + 1}
            let equalToken = EQUAL newPosition
            tokenizeHelper rest (equalToken :: acc) line (column + 2)
        | "," :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column}
            let newPosition = { startPosition with endColumn = column + 1}
            let commaToken = COMMA newPosition
            tokenizeHelper rest (commaToken :: acc) line (column + 2)
        | ":" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column}
            let newPosition = { startPosition with endColumn = column + 1}
            let colonToken = COLON newPosition
            tokenizeHelper rest (colonToken :: acc) line (column + 2)
        | "record" :: rest ->
            let startPosition = { startLine = line; startColumn = column; endLine = line; endColumn = column + 5 }
            let newPosition = { startPosition with endColumn = column + 6 }
            let recordToken = RECORD newPosition
            tokenizeHelper rest (recordToken :: acc) line (column + 7)
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
            elif regexDot.IsMatch(token) then 
                let dotToken = DOT newPosition
                tokenizeHelper rest (dotToken :: acc) line (column + 2)
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

    let matches = Regex.Matches(input, @"\b\w+\b|[=+\-*/<>(){},:.]+") 
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
        | VAR (v, _) :: DOT _ :: VAR (f, _) :: rest ->
            (RecDot (Var v, f), rest)
        | VAR (v, _) :: rest ->
            (Var v, rest)
        | LPAREN pos :: rest ->  // New case for left parenthesis
            let (expr, rest) = parseBinOp rest  // Call to parseBinOp
            match rest with
            | RPAREN _ :: rest -> (ParenExpr expr, rest)
            | _ -> failwith "Expected right parenthesis"
        | _ -> failwith "Unexpected token when parsing expression"

    and parseBinOp tokens =
        match tokens with
        | rest when rest |> List.head |> isExprToken ->
            let (expr1, rest) = parseExpr rest
            match rest with
            | OPERATE (op, _) :: rest ->
                let (expr2, rest) = parseExpr rest
                (Operate (op, expr1, expr2), rest)
            | _ -> failwith "Expected operator when parsing binary operation"
        | _ -> failwith "Unexpected token when parsing binary operation"

    let rec parseRecordField tokens =
        match tokens with
        | VAR (v, _) :: COLON _ :: rest -> 
            let (expr, rest) = parseExpr rest
            let field = (v, expr)
            match rest with 
            | COMMA _ :: rest -> 
                let (fields, rest) = parseRecordField rest
                ((field :: fields), rest)
            | _ -> ([field], rest)
        | _ -> failwith "Unexpected token when parsing record field"

    let rec parseStmt tokens =
        match tokens with
        | LET pos :: VAR (v, _) :: EQUAL _ :: rest ->
            let (expr, rest) = parseExpr rest
            (Let (Var v, expr), rest)
        | LET pos :: VAR (v, _) :: DOT _ :: VAR(v1, _) :: EQUAL _ :: rest ->
            let (expr, rest) = parseExpr rest
            (Let (Var v, expr), rest)
        | IF pos :: rest ->
            let (cond, rest) = parseExpr rest
            let thenStmt, rest = 
                match rest with
                    | THEN _ :: rest -> parseStmt rest
                    | _ -> failwith "Expected THEN token"
            let elseStmt, rest = 
                match rest with
                    | ELSE _ :: rest -> parseStmt rest
                    | _ -> failwith "Expected ELSE token"
            (If (cond, thenStmt, elseStmt), rest)
        | WHILE pos :: rest ->
            let (cond, rest) = parseExpr rest
            match rest with
            | DO _ :: rest -> 
                let (bodyStmt, rest) = parseStmt rest
                (While (cond, bodyStmt), rest)
            | _ -> failwith "Expected DO token after WHILE condition"
        | RECORD pos :: VAR (v, _) :: EQUAL _ :: rest -> 
            let (fields, rest) = parseRecordField rest
            (Record (v, (fields)), rest)
        | _ -> failwith "Unexpected token when parsing statement"

    fst (parseStmt tokens)