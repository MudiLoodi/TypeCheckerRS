module TypeInference

open TypeEnv
open AbSyn
exception TypeError of string 

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
    | Record (id, fields) -> 
        let recTypes = fields |> List.map (fun (name, expr) -> name, findtype tenv expr)
        Rec recTypes
    | RecDot (e1, f) -> 
        let e1_type = findtype tenv e1
        match e1_type with 
        | Rec recTypes -> 
            match List.tryFind (fun (name, _) -> name = f) recTypes with
            | Some (_, typ) -> typ
            | None -> raise (TypeError ("Field " + f + " not found in record"))
        | _ -> raise (TypeError ("Expression is not a record"))
    | ParenExpr (e1) -> 
        let e1_type = findtype tenv e1
        match e1_type with 
        | High -> High
        | Low -> Low
        | _ -> OK