#r "Microsoft.TeamFoundation.Build.Client"
#r "Microsoft.TeamFoundation.Client"
#r "Microsoft.TeamFoundation.VersionControl.Client"
#r "Newtonsoft.Json"

open System
open System.Diagnostics
open Microsoft.TeamFoundation.Build.Client

let runProc filename startDir args  = 
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )
        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
        let started = 
            try
                p.Start()
            with | ex ->
                ex.Data.Add("filename", filename)
                reraise()
        if not started then
            failwithf "Failed to start process %s" filename
        printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        timer.Stop()
        printfn "Finished %s after %A milliseconds" filename timer.ElapsedMilliseconds
        let cleanOut l = l |> Seq.filter (fun o -> System.String.IsNullOrEmpty o |> not)
        cleanOut outputs,cleanOut errors

let readOutputs (o,e) = 
            if e|> Seq.exists (fun e' -> String.IsNullOrWhiteSpace( e' ) = false) then
                o |> Seq.iter (printfn "%s")
                e |> Seq.iter (printfn "error:%s")
            else
                o |> Seq.iter (printfn "%s")
module TfExe = 
    let tfPath = @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\TF.exe"
    let tf startDir args = runProc tfPath startDir args

    // TfExe.tfAdd None [@"C:\tfs\XpressCharts2\BuildDefinitionBackups\FullBuild.json"];;
    let tfAdd basePath items =
        let addItem item = sprintf "add %s" item |> tf basePath
        items |> Seq.iter (addItem >> readOutputs)
    let tfCheckout basePath items = 
        let coItem item = sprintf "checkout \"%s\"" item |> tf basePath
        items |> Seq.iter (coItem >> readOutputs)
            

#if UseFsi
let tfs = TFS.GetTfs();
#else
#r @"C:\projects\fsi\tfsmacros.dll"
let tfs = Macros.Tfs.GetTfs(Uri("http://tfs20102:8080/tfs"))
#endif

type TeamProjectInfo = {Name:string; BuildDefinitionSavePath:string}

let teamProjects = [
    { Name="PracticeManagement"; BuildDefinitionSavePath = @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\BuildDefinitionBackups"}
    { Name="XpressCharts"; BuildDefinitionSavePath = @"C:\tfs\XpressCharts\BuildDefinitionBackups"}
]

let teamProjectCollection,build = 
    Macros.Build.GetBuildServer tfs
    |> (fun bs -> bs.TeamProjectCollection, Macros.TfsBuild(bs))

let teamProjectInfo = teamProjects |> Seq.skip 1 |> Seq.head

let builds = build.GetBuildDefinitions(teamProjectInfo.Name)
// let tp = vcs.GetTeamProjectForServerPath("$/Development");
let teamProjectCollectionUri = teamProjectCollection.Uri

let getBuildDefinitions() = build.GetBuildDefinitions(teamProjectInfo.Name)

module ExpressionHelpers = 
    open System.Linq.Expressions
    open System.Reflection
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.ExprShape
//
//    //method taken from http://stackoverflow.com/q/4604139/57883
//    let methodSourceName (mi:MemberInfo) =
//        mi.GetCustomAttributes(true)
//        |> Array.tryPick 
//                (function
//                    | :? CompilationSourceNameAttribute as csna -> Some(csna)
//                    | _ -> None)
//        |> (function | Some(csna) -> csna.SourceName | None -> mi.Name)
    let rec getName expr = 
        match expr with 
        |ShapeVar v -> v.Name
        |ShapeLambda (v,expr) -> v.Name
        |PropertyGet (_,p,_) -> p.Name
        |ShapeCombination (o, exprs) -> failwithf "not sure what to do with %A %A" o exprs

    let rec eval expr = // from http://stackoverflow.com/a/6404395/57883
        match expr with
        | Value(value,_) -> value //value
        | PropertyGet(Some(instance), pi, args) -> //instance property get
            pi.GetValue(eval instance, evalAll args) //notice recursive eval of instance expression and arg expressions
        | PropertyGet(None, pi, args) -> //static property get
            pi.GetValue(null, evalAll args)
        | Call(Some(instance), mi, args) -> //instance call
            mi.Invoke(eval instance, evalAll args)
        | Call(None, mi, args) -> //static call
            mi.Invoke(null, evalAll args)
        | _ -> failwith "invalid expression"
    and evalAll exprs = exprs |> Seq.map eval |> Seq.toArray

module JsonSerializationHelpers =
    open Newtonsoft.Json
    let private nameof expr = ExpressionHelpers.getName expr //|> (fun c -> c.Name)
    let private eval expr = ExpressionHelpers.eval expr
    type JsonObject = {Name:string; Properties:JsonProp seq} 
        and
        JsonProp = 
        |Expr of Quotations.Expr
        |ExprComplex of Quotations.Expr
        |NameValue of string*obj
        |PropObject of JsonObject
        |PropObjArray of string * JsonProp list seq
    let rec writeProperty (w:JsonTextWriter) prop =
        let inline writeProperty x = writeProperty w x 
        match prop with
        |Expr expr -> 
            w.WritePropertyName(nameof expr)
            w.WriteValue (eval expr)
        |ExprComplex expr ->
            w.WritePropertyName (nameof expr)
            let value = ExpressionHelpers.eval expr
            let value' = JsonConvert.SerializeObject(value)
            let deserialized = JsonConvert.DeserializeObject(value')
            w.WriteRawValue value'
        |NameValue (n,v) ->
            w.WritePropertyName n
            w.WriteValue v
        |PropObject jo ->
            w.WritePropertyName jo.Name
            w.WriteStartObject()
            jo.Properties |> Seq.iter writeProperty
            w.WriteEndObject()
        |PropObjArray (name,props) ->
            w.WritePropertyName name
            w.WriteStartArray()
            props |> Seq.iter (fun oProps -> 
                w.WriteStartObject()
                oProps |> Seq.iter writeProperty
                w.WriteEndObject()
                )
            w.WriteEndArray()

open JsonSerializationHelpers
let serializeBuild (teamProjectCollectionUri:string) (build:IBuildDefinition) = 
    use sw = new IO.StringWriter()
    use w = new Newtonsoft.Json.JsonTextWriter(sw)
    w.WriteStartObject()
    let nameof expr = ExpressionHelpers.getName expr

    let writeProperty prop = JsonSerializationHelpers.writeProperty w prop

    let trigger = 
        PropObject { 
            Name="Trigger"
            Properties = [
                            Expr <@ build.TriggerType @>
                            Expr <@ build.ContinuousIntegrationType @>
                            NameValue ("ContinuousIntegrationQuietPeriodMinutes",build.ContinuousIntegrationQuietPeriod)
                            Expr <@ build.BatchSize @>
        ]}
    let schedules = 
        build.Schedules 
        |> Seq.map (fun s ->
                    [
                        Expr <@ s.DaysToBuild @>
                        NameValue ("StartTimeSecondsPastMidnight", s.StartTime)
                        Expr <@ s.TimeZone @>
                    ]
            )
    let schedules = PropObjArray ("Schedules", schedules)
    let mappings =   
        build.Workspace.Mappings 
        |> Seq.map (fun m -> 
            [
                Expr <@ m.MappingType @>
                Expr <@ m.ServerItem @>
                Expr <@ m.LocalItem @>
                Expr <@ m.Depth @>
            ])
    let retentionPolicies = 
        build.RetentionPolicyList 
        |> Seq.map (fun rpl -> 
            [
                Expr <@ rpl.BuildReason @>
                Expr <@ rpl.BuildStatus @>
                Expr <@ rpl.DeleteOptions @>
                Expr <@ rpl.NumberToKeep @>
            ])

    [
        Expr <@ build.Uri @> // build.Uri
        Expr <@ build.Id @> //build.Id
        Expr <@ build.TeamProject @> //build.TeamProject
        NameValue ("CollectionUri",teamProjectCollectionUri)
        Expr <@ build.Name @> //build.Name
        Expr <@ build.Description @> //build.Description
        Expr <@ build.QueueStatus @>
        trigger
        schedules
        PropObject {Name="Workspace";Properties = [PropObjArray ("Mappings", mappings)]}
        PropObject {Name="BuildController";Properties=
            [
                Expr <@ build.BuildControllerUri @>
                Expr <@ build.BuildController.Name @>
                Expr <@ build.BuildController.Description @>
                Expr <@ build.BuildController.CustomAssemblyPath @>
            ]}
        Expr <@ build.DefaultDropLocation @>
        PropObject {Name="Process"; Properties = [
                                                Expr <@ build.Process.Id @>
                                                Expr <@ build.Process.Description @>
                                                Expr <@ build.Process.ServerPath @>
                                                Expr <@ build.Process.SupportedReasons @>
                                                Expr <@ build.Process.TeamProject @>
                                                Expr <@ build.Process.TemplateType @>
                                                ExprComplex <@ build.Process.Version @>
        ]}
        Expr <@ build.ProcessParameters@>
        PropObjArray ("RetentionPolicies", retentionPolicies)
    ] |> Seq.iter writeProperty
    w.WriteEndObject()
    build.Name,sw.ToString()

let buildToDump = build.GetBuildDefinition(teamProjectInfo.Name, "FullBuild")

let doCheckout targetPath = 
    if IO.File.Exists(targetPath) && IO.FileInfo(targetPath).Attributes.HasFlag IO.FileAttributes.ReadOnly then
        printfn "checking out from tfs"
        TfExe.tfCheckout None [targetPath]

// saveBuildDefinitions teamProjectInfo.BuildDefinitionSavePath [buildToDump];;
let saveBuildDefinitions savePath builds = 
    let paths = 
        builds
        |> Seq.map (serializeBuild (string teamProjectCollectionUri))
        |> Seq.map (fun (name,serialized) ->
            let deserialized = Newtonsoft.Json.JsonConvert.DeserializeObject(serialized)
            let json = Newtonsoft.Json.JsonConvert.SerializeObject(deserialized,Newtonsoft.Json.Formatting.Indented)

            let targetPath = IO.Path.Combine(savePath,name + ".json")
            doCheckout targetPath
            if System.IO.Directory.Exists savePath=false then
                IO.Directory.CreateDirectory(savePath) |> ignore
            IO.File.WriteAllText(targetPath,json)
            targetPath
        )
    paths |> printfn "Saved to %A"

