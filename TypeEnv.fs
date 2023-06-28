module TypeEnv

open AbSyn

type TypeEnv<'a> = TypeEnv of (string * 'a) list

exception MyError of string

let tenv = TypeEnv []

let rec lookup (n: string) (tenv: TypeEnv<'a>) =
    match tenv with
    | TypeEnv ttab ->
        match List.tryFind (fun (name, _) -> name = n) ttab with
        | Some (_, boundType) -> boundType
        | None -> failwith "Variable not found in the type environment"

// Binds a variable to a type 
let bind id btype (TypeEnv ttab) = TypeEnv ((id,btype)::ttab)

// Binds variables in an exp to types in the type enviornment
let rec bindExp e (TypeEnv envtab) = 
    match e with 
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
    | Num n -> bind (string n) OK (TypeEnv envtab)
