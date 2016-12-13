<Query Kind="FSharpProgram" />

// fable setup and play script - required admin access
// please note, almost every step/method in here modifies the %PATH% variable, which then doesn't take effect for this app until this application (or possibly app domain) is closed and re-opened

let uncurry f (x,y) = f x y
let delimit d items = String.Join(d,value=(items |> Array.ofSeq))
let dumpt (t:string) x = x.Dump(t); x
module Setup = 
    module Directory = 
        let rec recurseDirectory fIncludeDir fIncludeFileOpt fWithEx basePath = 
            let recurseDirectory = recurseDirectory fIncludeDir fIncludeFileOpt fWithEx
            
            seq {           
                match fIncludeFileOpt with
                | Some fIncludeFile -> 
                    let filesOpt = 
                        try
                            let files = Directory.GetFiles basePath |> Seq.choose fIncludeFile
                            Some files
                        with ex ->
                            fWithEx ex
                            None
                    match filesOpt with
                    | Some files -> 
                        yield! files
                    | None -> ()
                | None -> ()
                
                let children = 
                    try
                        let directories = Directory.GetDirectories basePath
                        let children = 
                            directories 
                            |> Seq.collect (fun d -> 
                                if fIncludeDir d then
                                    recurseDirectory d
                                else
                                    Seq.empty
                                )
                        children
                    with ex ->
                        fWithEx ex
                        Seq.empty
                yield! children
            }
        
        // swallow/ignore access denied, and other exceptions
        let tryDeleteAllChildren path = 
            let fWithEx _ = ()
            recurseDirectory (fun _ -> true) (Some (fun file -> Some file)) fWithEx path
            |> Seq.iter (fun fOrD ->
                match File.Exists fOrD, Directory.Exists fOrD with
                | true, _ -> File.Delete fOrD
                | false, true -> 
                    try
                        Directory.Delete(fOrD,true)
                    with ex -> fWithEx ex
                | false, false -> printfn "Could not delete %s, not found" fOrD
            )

    module Sec = 
        open System.Security.Principal
        let getIsAdmin() =
            WindowsIdentity.GetCurrent()
            |> WindowsPrincipal
            |> fun wp -> wp.IsInRole(WindowsBuiltInRole.Administrator)
        let requireAdmin () = 
            let runningAsAdmin = getIsAdmin()
            if not runningAsAdmin then
                failwithf "Can't alter hosts file without admin permissions"
                
    module Proc =
        let findCmd cmd = 
            try
                Util.Cmd(sprintf "where %s" cmd) |> ignore
            with ex -> 
                Util.OnDemand("cmdEx",fun () -> ex).Dump()
                Util.OnDemand("%PATH%", fun () -> 
                    let p = Environment.GetEnvironmentVariable "PATH" 
                    p.Split([| ';' |])).Dump()
        let showInExplorer path = 
            Process.Start("explorer.exe",sprintf "/select, \"%s\"" path)
                
    module Choc =             
        // install chocolatey
        let installChoco() = 
            Sec.requireAdmin()
            Util.Cmd """@powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin" """
        let testChocoInPath() = 
            // running choco without admin resulted in an error
            Sec.requireAdmin() 
            Util.Cmd "choco"
            
        let chocoNodeJs () = 
            Sec.requireAdmin()
            //Util.Cmd """choco install nodejs"""
            let psi = ProcessStartInfo("cmd"," /c \"choco install nodejs -y || pause\"")
            psi.CreateNoWindow <- false
            psi.UseShellExecute <- true
            
            let p = Process.Start(psi)
            p.Id.Dump("started process with id")
            
    module Npm = 
        let install globalOpt (beQuiet:bool) (packageNameOpt: string option) =
            let npmCall = sprintf "npm %s"
            let args = ["install"]
            match packageNameOpt,globalOpt with 
            | Some packageName,Some() -> args@["-g"; sprintf " %s" packageName]
            | Some packageName, None -> args@[sprintf " %s" packageName]
            | None, Some() -> args@["-g"]
            | None,None ->  args
            |> delimit " "
            |> npmCall
            |> dumpt "calling npm install"
            |> fun cmd -> Util.Cmd(cmd,beQuiet)
        let clearCache() = 
            Util.Cmd "npm cache clear"
        // derived from http://stackoverflow.com/a/15597395/57883
        let removeAll() = 
            let targetDir =
                let appData = Environment.ExpandEnvironmentVariables("%appdata%")
                Path.Combine(appData,"npm")
            printfn "Deleting %s" targetDir
            try
                Directory.Delete(targetDir,true)
            with ex ->
                ex.Dump()
                Directory.tryDeleteAllChildren targetDir
                
        let nukeIt() = 
            clearCache() |> ignore
            removeAll()
            if Sec.getIsAdmin() && Directory.Exists "node_modules" then
                Directory.tryDeleteAllChildren "node_modules"
//                Directory.Delete("node_modules",true)
            
        let installFable globalOpt =
            //Util.Cmd "npm install -g fable-compiler"    
            "fable-compiler"
            |> Some
            |> install globalOpt false

    module Search = 
        let findCommands () =
            Proc.findCmd "choco"
            Proc.findCmd "node"
            Proc.findCmd "npm"
            Proc.findCmd "fable"
Setup.Search.findCommands()

module Fable = 
    let compile outDirOpt scriptPath = 
        let fableCall args = sprintf "fable %s \"%s\"" args scriptPath
        match outDirOpt with
        | Some outDir -> sprintf "-o \"%s\"" outDir 
        | None -> String.Empty
        |> fableCall 
        |> Util.Cmd
    // assumes the outDir was not modified
    let locateOutput scriptPath = 
        scriptPath
        |> Path.GetDirectoryName 
        |> fun x -> Path.Combine(x, Path.GetFileNameWithoutExtension scriptPath + ".js")


let clean() = 
    Setup.Npm.nukeIt()
    
let scriptPath = @"C:\projects\javascript\Fable\HelloFable.fsx"    
let build() = 
    
    let targetDirectory = Path.GetDirectoryName scriptPath
    Environment.CurrentDirectory <- targetDirectory
    
    Environment.CurrentDirectory.Dump("cwd")
    Setup.Proc.findCmd "npm"
    Setup.Npm.install None false None |> dumpt "npm installer" |> ignore
    Fable.compile None scriptPath |> ignore
build()    
let outputFilePath = Fable.locateOutput scriptPath

if File.Exists outputFilePath then
    LINQPad.Hyperlinq(Action(fun () -> Setup.Proc.showInExplorer outputFilePath |> ignore<Process>), sprintf "explorer:%s" outputFilePath).Dump()



//Environment.CurrentDirectory.Dump()
//Util.Cmd "dir"