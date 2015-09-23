#r @"C:\projects\FsReveal\packages\FAKE\tools\FakeLib.dll"
open System
open Fake
#if INTERACTIVE
let oldDir = Environment.CurrentDirectory
let projDir =  @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\Pm.Dal/"
#else
let projDir = "./"
#endif
let buildDir = projDir + "fbuild/"
//Targets
Target "TargetDal" (fun _ ->
    let path = "Buildtime.fs"
    let orElse f s = if String.IsNullOrWhiteSpace(s) then f() else s
    let targetFile = IO.Path.Combine(projDir, path)
    let text = IO.File.ReadAllText(targetFile)
    let dataSource = 
        Environment.GetEnvironmentVariable("RwDb") 
        |> orElse (fun _ -> "Prog7-Pc") 
        |> sprintf "Data Source=%s;" 
    let replaced = Text.RegularExpressions.Regex.Replace(text,"Data Source=.*?;",dataSource)
    if text <> replaced then
        let attribs = IO.File.GetAttributes(targetFile)
        let notReadOnly = ~~~IO.FileAttributes.ReadOnly
        let attribs' = attribs &&& notReadOnly
        IO.File.SetAttributes(targetFile , attribs')
        IO.File.WriteAllText(targetFile , replaced)
)

Target "Clean" (fun _ -> 
    CleanDir buildDir
)
Target "BuildApp" (fun _ ->
    !! "./*.fsproj"
        |> MSBuildRelease buildDir "Build"
        |> Log "AppBuild-Output: "
)
Target "Default" ( fun _ -> 
    trace "Hello World from FAKE"
    trace System.Environment.CurrentDirectory
)


//Dependencies
"Clean"
    ==> "TargetDal"
    ==> "BuildApp"
    ==> "Default"

// Go

RunTargetOrDefault "Default"
