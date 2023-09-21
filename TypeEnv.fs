module TypeEnv

open AbSyn
type TypeEnv<'Type> = TypeEnv of (string * 'Type) list

let tenv = TypeEnv []

let rec lookup (n: string) (tenv: TypeEnv<'Type>) =
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
        let res1 = bindExp e1 (TypeEnv envtab) // Bind e1 to the environment
        let res2 = bindExp e2 res1  // Bind e2 to the updated type env res1
        res2
    | If (e1, e2, e3) ->
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        let res3 = bindExp e3 res2
        res3
    | Fun (e1, e2) -> 
        let localTEnv = []
        let res1 = bindExp e1 (TypeEnv localTEnv)
        let res2 = bindExp e2 res1
        match (envtab, res2) with
        | (ttab1, TypeEnv ttab2) ->
            let combinedTTab = ttab2 @ ttab1  // Concatenate the local tenv to the global tenv
            TypeEnv combinedTTab
    | App (e1, e2) ->
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | While (e1, e2) -> 
        let res1 = bindExp e1 (TypeEnv envtab)
        let res2 = bindExp e2 res1
        res2
    | Record (id, fields) -> 
        // Create a list of field types
        let fieldTypes = fields |> List.map (fun (fieldName, fieldExp) ->
            // Find the type of the current field expression.
            let res1 = bindExp fieldExp (TypeEnv envtab)
            // Extract the type environment list from the result
            let (TypeEnv envList) = res1
            // Get the type of the current field 
            let fieldType = snd (List.head envList)
            // Return a tuple of the field name and its inferred type.
            (fieldName, fieldType))
        // Bind the record id to the Rec type, which includes all the field types, and return the updated type environment.
        bind id (Rec fieldTypes) (TypeEnv envtab)
    | RecDot (e1, f) -> 
        (TypeEnv envtab)
    | ParenExpr (e1) -> 
        let res1 = bindExp e1 (TypeEnv envtab)
        res1