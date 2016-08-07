<Query Kind="FSharpProgram">
  <Connection>
    <ID>73195492-a414-4da3-b1c8-71d567112c5e</ID>
    <Persist>true</Persist>
    <Server>tcp:n6tkfzga4x.database.windows.net,1433</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>VIN_Billing_DEV</Database>
    <UserName>vinbillinguser</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAAZXI0kKLoAk26lXdoZmUdJQAAAAACAAAAAAAQZgAAAAEAACAAAACRqWukAaHeX1rQf4qT8p1RvEP5eX8E4o0kBAg2tqlXiwAAAAAOgAAAAAIAACAAAAA1crJOL/EQSfZmmlZibpdka2/eKVMod/Vs189p9uhcGRAAAACrc2m1O6A/OpJ+I9opQRMWQAAAAEwce2DVPInbz7z6z+aGdABybG/lnEuPfot5B9HfQaB/EWyqCJUQvJtUX9ScCV4m/brYgUqmSOWJu3nqSNfsboU=</Password>
    <DbVersion>Azure</DbVersion>
  </Connection>
</Query>

let dc = new TypedDataContext()
let (|TypeDefOf|_|) (_:'a) t = 
    if t = typedefof<'a> then Some() else None
let (|TypeOf|_|) (_:'a) t  = 
    if t = typeof<'a> then Some ()
    else 
        //printfn "did not match %A to %A" typeof<'a> t ; 
        None

let isType<'a> = Unchecked.defaultof<'a>

let mapTypeToF = 
    function
    | TypeOf(isType:int) -> "int"
    | TypeOf(isType:string) -> "string"
    | TypeOf(isType:Nullable<DateTime>) -> "DateTime Nullable"
    | TypeOf(isType:Nullable<int>) -> "int Nullable"
    | TypeOf(isType:Nullable<bool>) -> "bool Nullable"
    | x -> x.Name
    
[<AutoOpen>]
module StringAssembler = //Abstract Data Type
    type Sb = 
        private 
            { SB:StringBuilder}
        with
            override x.ToString() = x.SB.ToString()
            
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Sb =
        let startAssembler() = {SB = StringBuilder()}
        let appendLine i (s:string) (sb:Sb) =
            let addlIndent = Enumerable.Repeat("  ",i) |> Seq.fold (+) String.Empty
            sb.SB.AppendLine(sprintf "%s%s" addlIndent s) |> ignore
            sb
module SimplifiedInput =
    type StringLength= |Length of int | UseMax
    type NumberDetails = { Scale:int;Precision:int}
    type DbType =
        | String of StringLength
        | Money of NumberDetails
        
    type ColumnInfo = {Name:string; Type:Type; Length: int option; Precision: int option; Scale: int option; UseMax:bool}
    type TableInfo = {Name:string; Schema: string; Columns: ColumnInfo seq}
    

module ModelGenerator = 
    let extractTypeInfo (type':Type) = 
        type'
        |> fun t -> t.GetProperties()
        |> Seq.filter (fun p -> p.Name.StartsWith("_") |> not)
        |> Seq.filter (fun p -> p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() = typedefof<Table<_>> )
        |> Seq.map (fun p -> p.Name, p.PropertyType.GenericTypeArguments.[0])
        |> Seq.map (fun (name,t) -> name, t.Name, t.GetFields() |> Seq.map(fun f -> f.Name, f.FieldType))
    
    type GenerationStyle = 
        |Readonly
        |Writable
        
    type GenerationType =
        |Value
        |Entity
    
    let generateInterface gs sb (indent:string) (name:string) (props:seq<string*Type>) =
        
        let addProps i sb = 
            let suffix = match gs with |Readonly -> String.Empty |Writable -> " with get,set"
            props 
            |> Seq.map (fun (name,t) -> 
                sprintf "/// %s" t.FullName, sprintf "abstract member %s:%s%s" name (mapTypeToF t) suffix
                )
            |> Seq.iter (fun (comment,text) -> 
                sb
                |> Sb.appendLine i comment
                |> Sb.appendLine i text 
                |> ignore
                )
            sb
            
        let iname = match gs with |Readonly -> "" |Writable -> "RW"
        
        sb 
        |> Sb.appendLine 0 String.Empty
        |> Sb.appendLine 1 (sprintf "type I%s%s =" iname name)
        |> addProps 2
        
//let generateRecord typeName 

let limit = 5
let sb = Sb.startAssembler()
let types = 
    Util.Cache(fun () -> dc.GetType()) 
    |> ModelGenerator.extractTypeInfo
    |> Seq.take 5
    |> Array.ofSeq
    
Newtonsoft.Json.JsonConvert.SerializeObject(types).Dump("shape input")

try
    types
    |> Seq.map (fun (_,name,t) -> 
        name, ModelGenerator.generateInterface ModelGenerator.Readonly sb String.Empty name t)
    |> Seq.iter (fun (name,s)-> (s.ToString()).Dump(name))
finally
    types
    |> Dump
    |> ignore
    