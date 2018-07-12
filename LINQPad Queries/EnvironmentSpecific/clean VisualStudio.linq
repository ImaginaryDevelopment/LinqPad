<Query Kind="FSharpProgram" />

// clean visual studio
// track all the things you might do to clean up visual studio ... iis cache, designer cache, etc.

// most of these will assume you have closed devenv.exe

// made on windows 10 with VS 2017 for assumptions/examples to code by

//type RelativePath =
//    | Raw of string
//    | Wild of string
type RelativePath = |RelativePath of string with
    member x.Value = match x with | RelativePath x -> x
    
module Helpers =
    let flip f x y = f y x
    let startsWithI (d:string) (x:string) =
        x.StartsWith(d,StringComparison.CurrentCultureIgnoreCase)
    let inline contains (d:'a) (x:^t) =  (^t:(member Contains:'a -> bool)(x,d))

    let tryIter f =
       Seq.iter(fun x ->
        try
            f x
        with _ ->
            ()
       )
    module Path =
        let combine (RelativePath sd) d = Path.Combine(d,sd)
        let splitWild sd x =
            combine sd x
            |> fun x -> x.Split([| '*' |],count=2)
            |> function | [|h;wild|] -> (h, wild)
            
        let rec getSubs (sd:RelativePath) x =
//            printfn "Checking %s is in %s" sd x
            let results = 
                let fp =  combine sd x
                if fp.Contains "*" then
                    let basePath,remainder =  splitWild sd x
                    // trim leading \
                    let remainder = remainder.[1..]
                    if not <| Directory.Exists basePath then
                        Seq.empty
                    else 
                        let wildResult = 
                            Directory.GetDirectories basePath
                        (wildResult,basePath,remainder,sd,x).Dump("wildcarded")
                        wildResult
                        |> Seq.collect (getSubs (RelativePath remainder))
                else 
                    [fp]
                    |> Seq.filter Directory.Exists
                    
            results
        let getSubs' (sds:_ seq) x =
            sds
            |> Seq.collect (flip getSubs x)
                
                
                
    module Option =
        let ofDirectoryExists f = 
            if Directory.Exists f then
                Some f
            else None
            
        
        let ofSubDirectory sd x =
            Path.combine sd x
            |> ofDirectoryExists
open Helpers
    
    
let hasSubFolder sd =
    Option.ofSubDirectory sd
    >> Option.isSome
    
let findVsCommon7IdePath () = 
    Set [ Environment.GetFolderPath Environment.SpecialFolder.ProgramFiles; Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86; Environment.ExpandEnvironmentVariables "%ProgramW6432%"]
    |> Seq.filter(Directory.Exists)
    |> Seq.collect Directory.GetDirectories
    |> Seq.filter (Path.GetFileName >> startsWithI "Microsoft Visual Studio")
    // VS 2017 starts having a version / edition subfolder
    |> Seq.collect(Path.getSubs' [ RelativePath "Common7\\IDE";RelativePath "2017\\*\\Common7\\IDE"])
//    |> chooseSubFolder "Common7\\IDE"

let getVsRootLocalDataFolder sub =
    let path = Environment.ExpandEnvironmentVariables("%localappdata%/Microsoft/VisualStudio/")
    let installations = 
        Directory.GetDirectories(path) 
        // assume all relevant subdirs start with 1 as in 14.0 or 15.0
        |> Array.filter(fun c -> Path.GetFileName c |> fun x -> x.[0] |> Char.IsNumber && hasSubFolder sub c)
    installations
    
// kill everything inside the folder, not the folder itself
let tryWipe path = 
    if not <| Directory.Exists path then
        None
    else 
        Directory.GetDirectories path
        |> tryIter Directory.Delete
        Directory.GetFiles path
        |> tryIter File.Delete
        Some ()
let cleanDesignerCache fFolderSelection = 
    let targetSub = RelativePath "Designer"
    getVsRootLocalDataFolder targetSub
    |> fFolderSelection
    |> Option.map (fun target -> Path.combine targetSub target |> Path.GetFullPath)
    |> Option.iter (fun f -> 
            let cache = Path.Combine(f,"ShadowCache")
            if Directory.Exists cache then
                tryWipe cache
                |> Option.iter(fun _ -> 
                    
                    printfn "Wiped ShadowCache from %s" f)
    )
            
let clearComponentCache fFolderSelection = 
    let targetSub = RelativePath "ComponentModelCache"
    getVsRootLocalDataFolder targetSub
    
    |> Array.filter(Directory.GetDirectories >> Array.exists(fun subDir -> subDir.EndsWith targetSub.Value))
    |> fFolderSelection
    |> Option.map (fun target -> Path.combine targetSub target |> Path.GetFullPath)
    |> Option.iter (fun f -> 
            if Directory.Exists f then
                tryWipe f
                |> Option.iter(fun _ -> 
                    
                    printfn "Wiped ComponentCache from %s" f)
    )
// https://blogs.msdn.microsoft.com/willy-peter_schaub/2010/09/15/if-you-have-problems-with-tfs-or-visual-studio-flush-the-user-cache-or-not/
// untested (accidently reset my vs 2015, so that part is tested) (untested with 2017)
let resetUserData fFolderSelection =
    // in linqpad 32 bit it seems both GetFolderPath args return the x86 folder
    // 2017 
    findVsCommon7IdePath()
    |> fFolderSelection
    |> Option.map (fun folder ->
        
        ProcessStartInfo("devenv.exe","/resetusercache", WorkingDirectory=folder)
//        |> Process.Start
    )
        
cleanDesignerCache (Seq.rev >> Seq.tryHead)
//clearComponentCache (Seq.rev >> Seq.tryHead)
//resetUserData Seq.tryHead
|> Dump
|> ignore