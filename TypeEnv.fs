module TypeEnv

open AbSyn

type TypeEnv<'a> = TypeEnv of (string * 'a) list

let tenv = TypeEnv []

let rec lookup (n: string) (tenv: TypeEnv<'a>) =
    match tenv with
    | TypeEnv ttab ->
        match List.tryFind (fun (name, _) -> name = n) ttab with
        | Some (_, boundType) -> boundType
        | None -> failwith $"Variable '{n}' not found in the type environment"

// Binds a variable to a type 
let bind id btype (TypeEnv ttab) = TypeEnv ((id,btype)::ttab)

// Binds variables in an exp to types in the type enviornment
let rec bindExp e (TypeEnv envtab) = 
    match e with 
    | Num n -> bind (string n) OK (TypeEnv envtab)
    | Var e1 -> 
        if e1.StartsWith("H_") then
            bind e1 High (TypeEnv envtab)
        else
            bind e1 Low (TypeEnv envtab)
    | Operate (op, e1, e2) ->
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | Let (e1, e2) -> 
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | If (e1, e2, e3) ->
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        let res3 = bindExp e3 res2
        res3
    | Fun (e1, e2) -> 
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | App (e1, e2) ->
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | While (e1, e2) -> 
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | Record (fields) -> 
        let bindField typeEnv (fieldName, fieldValue) =
            let res1 = bindExp fieldValue typeEnv // Find the type of the field value
            let (TypeEnv envList) = res1
            let fieldType = snd (List.head envList)
            bind fieldName fieldType typeEnv // Bind the field to the type of the field value in the type env
        List.fold bindField (TypeEnv envtab) fields // Bind each field in the list
    | RecDot (e1, f) -> 
        (TypeEnv envtab)
    | ParenExpr (e1) -> 
        let res1 = bindExp e1 (TypeEnv envtab)
        res1