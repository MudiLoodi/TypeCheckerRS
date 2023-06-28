module main

open TypeEnv
open AbSyn


let rec hastype tenv e =
    match e with
    | Num n -> OK
    | Var(e1) -> 
        let res = lookup e1 tenv
        match res with
            | High  -> High
            | Low   -> Low
    | Operate (op, e1, e2) ->
        let e1_type = hastype tenv e1
        let e2_type = hastype tenv e2
        match (e1_type, e2_type) with
            | (High, _) | (_, High) -> High
            | (Low, Low)            -> Low
            | (Low, OK)             -> Low
    | Let (e1, e2) -> 
        let e1_type = hastype tenv e1
        let e2_type = hastype tenv e2
        match(e1_type, e2_type) with 
            | (High, _)     -> High
            | (Low, Low)    -> Low
            | (Low, OK)     -> Low
            | (Low, High)   ->  raise (MyError ($"Illegal explicit flow to %A{e1}"))
    | If (e1, e2, e3) ->
        let e1_type = hastype tenv e1
        let e2_type = hastype tenv e2
        let e3_type = hastype tenv e3
        match (e1_type, e2_type, e3_type) with 
            | (High, High, High) -> High
            | (High, Low, Low) -> raise (MyError($"Illegal Implicit Flow from then-branch: %A{e2_type}, else-branch: %A{e3_type} to clause: %A{e1_type}"))
            | (Low, _, _) -> Low


let exp = Let (Var "H_a", Num 2)
let exp1 = Let (Var "H_a", Num 5)

let cond = Operate (Equal, Var "f", Num 1)

let ifexp = If (cond, exp, exp1)
let program = [exp; exp1; cond; ifexp]

let finalTenv =
    program
    |> List.fold (fun accumulatedTenv currentExp -> bindExp currentExp accumulatedTenv) tenv
    |> (fun (TypeEnv ttab) -> TypeEnv (List.rev ttab))

let (TypeEnv EnvLst) = finalTenv

printfn "%A" (hastype finalTenv ifexp)