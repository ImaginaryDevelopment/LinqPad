#r "Microsoft.TeamFoundation.Build.Client"
#r "Microsoft.TeamFoundation.Client"
#r "Microsoft.TeamFoundation.VersionControl.Client"
#r "Newtonsoft.Json"

open System
open System.Diagnostics
open Microsoft.TeamFoundation.Build.Client

let runProc filename args startDir = 
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
    let started = p.Start()
    if not started then
        failwithf "Failed to start process %s" filename
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()
    outputs,errors

module TfExe = 
    let tf startDir args = 
        runProc @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\TF.exe" args startDir

    let tfAdd basePath items =
        let addItem item = sprintf "add %s" item |> tf basePath
        let readOutputs (o,e) = 
            if e|> Seq.exists (fun e' -> String.IsNullOrWhiteSpace( e' ) = false) then
                o |> Seq.iter (printfn "%s")
                e |> Seq.iter (printfn "error:%s")
            else
                o |> Seq.iter (printfn "%s")
        items |> Seq.iter (addItem >> readOutputs)

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

module LiteralSerializer = // holds the direct translated C# code and types 
    type BuildDefinition = {
        Uri: Uri
        Id:string
        TeamProject:string
        CollectionUri:Uri
        Name:string
        Description:string
        QueueStatus: DefinitionQueueStatus
        Trigger:Trigger
        Schedules: Schedule[]
        Workspace: Workspace
        BuildController:BuildController
        DefaultDropLocation:string
        Process:ProcessTemplate
        ProcessParameters:string
        RetentionPolicies:RetentionPolicy[]
        } and
        Trigger = {
            TriggerType: DefinitionTriggerType
            ContinuousIntegrationType: ContinuousIntegrationType
            ContinuousIntegrationQuietPeriodMinutes: int
            BatchSize:int
        } and
        Schedule = {
            DaysToBuild:ScheduleDays
            StartTimeSecondsPastMidnight: int
            TimeZone: TimeZoneInfo
            Type: ScheduleType
        } and
        Workspace = { Mappings:Mapping[] } 
        and
        Mapping = {
            MappingType:WorkspaceMappingType
            ServerItem:string
            LocalItem:string
            Depth:WorkspaceMappingDepth
        } and
        BuildController = {
            BuildControllerUri:Uri
            Name:string
            Description:string
            CustomAssemblyPath:string
        } and
        ProcessTemplate = {
            Id:int
            Description:string
            ServerPath:string
            SupportedReasons:BuildReason
            TeamProject:string
            TemplateType:ProcessTemplateType
            Version:Version
        } and
        RetentionPolicy = {
            BuildReason:BuildReason
            BuildStatus:BuildStatus
            DeleteOptions: DeleteOptions
            NumberToKeep:int
        }


    let mapBuild (buildToDump:IBuildDefinition)= 
        {
            Uri=buildToDump.Uri
            Id= buildToDump.Id
            TeamProject = buildToDump.TeamProject
            CollectionUri = teamProjectCollectionUri
            Name = buildToDump.Name
            Description = buildToDump.Description
            QueueStatus = buildToDump.QueueStatus
            Trigger = 
                {
                    TriggerType = buildToDump.TriggerType
                    ContinuousIntegrationType = buildToDump.ContinuousIntegrationType
                    ContinuousIntegrationQuietPeriodMinutes = buildToDump.ContinuousIntegrationQuietPeriod
                    BatchSize = buildToDump.BatchSize
                }
            Schedules = 
                buildToDump.Schedules 
                |> Seq.map (fun s -> { DaysToBuild = s.DaysToBuild; StartTimeSecondsPastMidnight = s.StartTime; TimeZone = s.TimeZone; Type = s.Type })
                |> Array.ofSeq
            Workspace = { Mappings= buildToDump.Workspace.Mappings |> Seq.map (fun m -> {MappingType = m.MappingType; ServerItem = m.ServerItem; LocalItem = m.LocalItem; Depth = m.Depth}) |> Array.ofSeq}
            BuildController = {BuildControllerUri = buildToDump.BuildControllerUri; Name = buildToDump.BuildController.Name; Description = buildToDump.BuildController.Description; CustomAssemblyPath = buildToDump.BuildController.CustomAssemblyPath}
            DefaultDropLocation = buildToDump.DefaultDropLocation
            Process= 
                {
                    Id = buildToDump.Process.Id
                    Description = buildToDump.Process.Description
                    ServerPath = buildToDump.Process.ServerPath
                    SupportedReasons = buildToDump.Process.SupportedReasons
                    TeamProject = buildToDump.Process.TeamProject
                    TemplateType = buildToDump.Process.TemplateType
                    Version = buildToDump.Process.Version
                }
            ProcessParameters= buildToDump.ProcessParameters
            RetentionPolicies = buildToDump.RetentionPolicyList |> Seq.map (fun rpl -> { BuildReason = rpl.BuildReason; BuildStatus = rpl.BuildStatus; DeleteOptions = rpl.DeleteOptions; NumberToKeep = rpl.NumberToKeep}) |> Array.ofSeq
        }

    let buildToDump = build.GetBuildDefinition(teamProjectInfo.Name, "FullBuild")
    let saveBuildDefinitions savePath builds = 
        let paths = 
            builds
            |> Seq.map mapBuild
            |> Seq.map (fun b ->
                let json = Newtonsoft.Json.JsonConvert.SerializeObject(b,Newtonsoft.Json.Formatting.Indented)
                let targetPath = IO.Path.Combine(savePath,b.Name + ".json")
                if System.IO.Directory.Exists savePath=false then
                    IO.Directory.CreateDirectory(savePath) |> ignore
                IO.File.WriteAllText(targetPath,json)
                targetPath
            )
        paths |> printfn "Saved to %A"

