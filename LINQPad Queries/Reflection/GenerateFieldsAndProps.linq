<Query Kind="FSharpProgram">
  <Connection>
    <ID>4e94eacc-a31d-4687-947b-e4c9804c895a</ID>
    <Persist>true</Persist>
    <Server>(local)</Server>
    <IncludeSystemObjects>true</IncludeSystemObjects>
    <Database>PmRewriteApplicationDatabase</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <Reference>C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\Pm.Dal\bin\Debug\Pm.Dal.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

let dc = new TypedDataContext()

type NullableBehavior = |UseOpt |UseNullable
type ``C#Outs`` = | Props |InterfaceProps
type DesiredOutput = | ``C#`` of ``C#Outs`` | ``F#Record`` of NullableBehavior
let transformCType (type':Type) = 
    if type'.Name.StartsWith("Nullable") then
        type'.GetGenericArguments().[0].Name + "?"
    else
        type'.Name
let makeTypePart desiredOutput (t:Type)= 

    let fields = 
        t.GetFields()
        |> Seq.filter (fun f ->f.IsPublic)
        |> Seq.map (fun f-> f.FieldType,f.Name )
    let props = 
        t.GetProperties()
        |> Seq.map (fun p -> p.PropertyType, p.Name)
    let combined = Seq.concat [fields; props]
    match desiredOutput with
    | ``C#`` outStyle ->
        combined |>
        match outStyle with 
        | Props ->
    	    Seq.map (fun (t,name) -> sprintf "public %s %s {get;set;}\r\n" (transformCType t) name)
        |InterfaceProps ->
            Seq.map (fun (t,name) -> sprintf "%s %s {get;set;}\r\n" (transformCType t) name)
    | ``F#Record`` nb ->
        let transformType (type':Type) = 
            if type'.Name.StartsWith("Nullable") then
                match nb with 
                |UseNullable -> type'.GetGenericArguments().[0].Name + " Nullable"
            else
                type'.Name
        combined
        |> Seq.map (fun (t,name) -> sprintf "%s : %s\r\n" name (transformType t))
let testTypes = [
    "LocalLinq",typeof<PaymentsTemp> // local linq to sql class
    "DalLinq",typeof<Pm.Dal.Encounter.dbEncounter.ServiceTypes.PaymentsTemp>
]
testTypes
|> Seq.iter (fun (title,t) -> 
                [
                    makeTypePart (``C#`` Props) t
                    makeTypePart (``C#`` InterfaceProps) t
                    makeTypePart (``F#Record`` UseNullable) t
                ]
                |> (fun items -> Util.HorizontalRun ("C#Props,C#Interface,F#Record", items))
                |> (fun hr -> hr.Dump(title + ":" + t.Name))
            )


