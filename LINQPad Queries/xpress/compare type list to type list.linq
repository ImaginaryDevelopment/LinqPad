<Query Kind="FSharpProgram">
  
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\Pm.Dal.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\Pm.Schema.dll</Reference>
  <Reference>C:\TFS\PracticeManagement\dev\PracticeManagement\bin\PracticeManagement.Foundation.dll</Reference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
</Query>

let dc = new TypedDataContext()
type MetaDataSource = |Type | ClassMethodName of string | ModuleMethod of string*string // doesn't account for overloads

let getInfo (typeRef:Type) source = 
    let getMethodParams (mi:MethodInfo) = 
        mi.GetParameters()
        |> Seq.map (fun p -> p.Name, p.ParameterType)
    match source with
    | ClassMethodName name ->
        typeRef.GetMethods ()
        |> Seq.find (fun m -> m.Name = name)
        |> getMethodParams
    | Type -> 
        let props = typeRef.GetProperties() |> Seq.map (fun p -> p.Name, p.PropertyType) |> Array.ofSeq
        let fields = typeRef.GetFields() |> Seq.map (fun f -> f.Name, f.FieldType) |> Array.ofSeq
        Seq.concat [props;fields]
    | ModuleMethod (moduleName,methodName) ->
        typeRef.Assembly.GetTypes() 
        |> Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule 
        |> Array.find (fun t -> t.Name = moduleName) 
        |> (fun t -> t.GetMethods())
        |> Seq.find(fun m -> m.Name = methodName)
        |> getMethodParams

let source = getInfo typeof<LINQPad.User.Codes> MetaDataSource.Type
let target = getInfo typeof<PracticeManagement.Foundation.DataModels.CodeDataModel> MetaDataSource.Type
//target.Dump()

        
let missingFromSource = 
    query {
        for (sName,sType) in source do
        leftOuterJoin tpl in target on ((sName.ToLower()) = ((fst tpl).ToLower())) into tpLeft
        for tp in tpLeft.DefaultIfEmpty() do
        where (box tp = null)
        select (sName,sType.Name)
    }

//missingFromSource.Dump("missing")
//missingFromSource.Dump("missing");

let missingFromTarget = 
    query{
        for (tName,tType) in target do
        leftOuterJoin spl in source on ((tName.ToLower()) = ((fst spl).ToLower())) into spLeft
        for sp in spLeft.DefaultIfEmpty() do
        where (box sp = null)
        select (tName,tType.Name)
    }
    
Util.HorizontalRun("missingFromTarget,missingFromSource", missingFromSource,missingFromTarget).Dump()
let mismatched = 
    query {
        for (tName,tType) in target do
        join (sName,sType) in source on (tName.ToLower() = sName.ToLower())
        where (sType.Name <> tType.Name)
        sortBy sName
        select (sName,sType,tType)
    }
    
mismatched.Dump("mismatches")