<Query Kind="FSharpProgram" />

// copy and launch release drop

//let dc = new TypedDataContext()
//dc.Connection.ConnectionString.Dump()

module Helpers =
    let dumpt (t:string) x = x.Dump(); x
    let rec cp sourceDir targetDir =
        printfn "Copying from %s to %s" sourceDir targetDir
        for f in Directory.GetFiles(sourceDir) do
            let target = Path.Combine( targetDir, Path.GetFileName(f))
    //        if File.Exists target then
    //            File.Delete(target)
            File.Copy(f, target)
        for d in Directory.GetDirectories(sourceDir) do
            let dirName = Path.GetFileName(d)
            let target = Path.Combine( targetDir, dirName)
            let sourceDir = Path.Combine(sourceDir, dirName)
    //        Directory.Delete(target,true)
            Directory.CreateDirectory(target) |> ignore
            cp sourceDir target
            
    let dumpKillLinq (p:Process) = // TODO: try to get the liveKillLink:observable<HyperLinq> deal working
                let killIt () = 
                    p.Id.Dump("killing process")
                    p.Kill()
                let h = Hyperlinq(Action(killIt),sprintf "Kill Process:%i" p.Id, runOnNewThread = true)
                h.Dump()
                
    let copyDrop selection target = 
        let sourceDir = Path.Combine(selection, "drop")
        if not <| Directory.Exists sourceDir then
            failwithf "unable to locate drop folder at %s for %s" sourceDir selection
        cp sourceDir target
    
open Helpers 

let dirs = Directory.EnumerateDirectories(@"\\fs01\builds\PM\Pm_VsBuild_Release_Deploy\") |> Seq.sort |> List.ofSeq |> List.rev
dirs.Dump()
//
let selection = Util.ReadLine("selected release?", dirs.[0], dirs)
if not <| Directory.Exists selection then
    failwithf "Unable to verify existence of selection %s" selection
//let year,month,day,build = Regex.Match(selection, @"(\d{4})(\d{2})(\d{2})\.(\d+)") |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value)
let target = 
        let targetParent = @"C:\TFS\PracticeManagement\dev\PracticeManagement\Releases"
        let target = Path.Combine(targetParent, Path.GetFileName selection)
        target
        

if not <| Directory.Exists target then
    Directory.CreateDirectory(target)
    copyDrop selection target
    
else printfn "Skipping copy, target already exists %s" target

let launchTarget = 
    let files = 
        Directory.GetFiles(target, "*.exe", SearchOption.AllDirectories)
        |> dumpt "executables in target"
    if files |> Seq.exists(fun _-> true) then
        files
        |> Seq.tryFind(fun f -> f.EndsWith("PracticeManagement.exe"))
        |> Option.map (fun f -> Process.Start(f), f)
        |> Option.map (fun (p,f) -> dumpKillLinq p; p.Id, f)
    else
        failwithf "Could not find any executables in %s" target

launchTarget

