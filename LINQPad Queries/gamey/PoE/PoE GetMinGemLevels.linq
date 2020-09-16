<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>PathOfSupporting</NuGetReference>
  <Namespace>PathOfSupporting.Parsing.Trees</Namespace>
  <Namespace>PathOfSupporting.Parsing.Trees.Gems</Namespace>
</Query>

let getGemPath () =
    let temp = Path.GetTempPath()
    let dir = Path.Combine(temp,"linqpad","PathOfSupporting")
    let fullTarget = Path.Combine(dir,"Gems.json")
    if not <| File.Exists fullTarget then
        let src = "https://github.com/ImaginaryDevelopment/PathOfSupporting/raw/master/PoS/Gems3.5.json"
        let hc = new System.Net.Http.HttpClient()
        let text = hc.GetStringAsync src |> Async.AwaitTask |> Async.RunSynchronously
        text.[0..300].Dump(fullTarget)
        if not <|Directory.Exists dir then
            Directory.CreateDirectory dir |> ignore
        File.WriteAllText(fullTarget,text)
    fullTarget
    
    
    // localappdata/nugetfw4.6
//    let p = Path.Combine(Environment.ExpandEnvironmentVariables("%localappdata%"),"linqpad","nuget.fw46","PathOfSupporting")
//    Directory.GetFiles(p,"*.json").Dump("json")
//    
//    Directory.GetDirectories(p).Dump("dirs")
//    Directory.GetFiles(p).Dump("files")
    
//    let t = typeof<PathOfSupporting.Schema.Apis.FetchArguments<_>>
//    let ass = t.Assembly
//    ass.GetManifestResourceNames().Dump("rns")
//    let fp = ass.Location
//    System.Resources.ResourceManager(t).Dump()
    
match PathOfSupporting.Parsing.PoB.PathOfBuildingParsing.processCodeOrPastebin(System.Windows.Forms.Clipboard.GetText()) with //("https://pastebin.com/sCMfDMHe") with
| Error e -> e.Dump(); None
| Ok ch ->
    let skillIds =
        ch.Skills.SkillGroups
        |> Seq.collect(fun sk -> sk.Gems)
        |> Seq.map(fun sk -> sk.Name)
        |> Seq.sort
        |> Seq.distinct
    let gp =getGemPath()
    let result =
        PathOfSupporting.Parsing.Trees.Gems.getGemReqLevels {JsonResourcePath.ResourceDirectory=Path.GetDirectoryName gp;Filename = Some <| Path.GetFileName gp} skillIds
        |> function
            |Ok n -> Some n
            |Error e -> e.Dump(); None
    result
|> Option.map (fun v -> v |> Seq.sortBy(fun x -> x.LevelRequirement))
|> Option.iter (fun x -> x.Dump() )