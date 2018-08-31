<Query Kind="FSharpExpression">
  <Reference>C:\tfs\practicemanagement\trunk\bin\Pm.Schema.dll</Reference>
</Query>

// reflect over generated sproc file, test all sprocs in begin tran/rollback
let (|ValueString|NonValueString|) =
    function
    | null -> NonValueString
    | "" -> NonValueString
    | x when String.IsNullOrWhiteSpace x -> NonValueString
    | x -> ValueString x

let (|Contains|_|) (d:string) =
    function
    | NonValueString -> None
    | x when x.Contains(d) -> Some x
    
let (|Before|_|) (d:string) =
    match d with
    | null | "" -> invalidOp <| sprintf "delimiter %s is invalid" (if isNull d then "null" else sprintf "'%s'" d)
    | _ -> ()
    function
    | NonValueString -> None
    | ValueString x when x.Contains(d) -> x.[0.. x.IndexOf d - d.Length] |> Some
    | _ -> None
let (|BeforeAnyOf|_|) (d:char[]) =
    function
    | ValueString x ->
        match x.LastIndexOfAny(d) with
        | i when i >= 0 -> x.[0.. i-1] |> Some
        | _ -> None
    | _ -> None

let (|AfterAllOf|_|) (d:char[]) =
    function
    | ValueString x ->
        match x.LastIndexOfAny(d) with
        | i when i >= 0 -> x.[i+1..] |> Some
        | _ -> None
    | _ -> None
let getTypeParentName =
    function
    | BeforeAnyOf [| '+';'.' |] x -> Some x
    | _ -> None
let getName =
    function
    | AfterAllOf [| '+';'.' |] x -> Some x
    | _ -> None

let isObjectMethod (m:MethodInfo) = m.DeclaringType = typeof<obj>
let t' =  typeof<Pm.Schema.DataModels.StoredProcedures.Dbo.GetAdjustmentReasonsInput>
let scrapeAsMethods =
    Seq.choose(fun (t:Type) ->
        match t.GetMethods() |> Array.filter(isObjectMethod >> not) |> Array.filter(fun t -> t.ReturnType = typeof<string>) with
        | [| |] -> None
        | x -> Some(t.FullName,x |> Array.map(fun t -> t.Name))
    )
// relies on having a model or namespace with the name "DataModels.StoredProcedures"
let getSprocs (asm:Assembly) =
    asm.GetExportedTypes()
    |> Seq.filter(fun t -> t.FullName.Contains("DataModels.StoredProcedures"))
    |> Seq.collect(fun t -> 
        t.GetProperties() |> Seq.map(fun f -> sprintf "%s.%s" t.Name f.Name)
    )
    
    
t'.Assembly

|> getSprocs
//|> Seq.map (fun t -> t.Name)
