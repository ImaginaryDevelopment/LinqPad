<Query Kind="FSharpProgram">
  <Reference>&lt;CommonApplicationData&gt;\LINQPad\Updates50\beta\LINQPad.exe</Reference>
  <NuGetReference>System.IO.Compression</NuGetReference>
  <NuGetReference>System.IO.Compression.ZipFile</NuGetReference>
  <Namespace>System.IO.Compression</Namespace>
  <DisableMyExtensions>true</DisableMyExtensions>
  <CopyLocal>true</CopyLocal>
</Query>

// iterate a build drop structure, getting a specific file's version info
// consider: adding the ability to query changeset number

let toCharArray(x:string) = x.ToCharArray()
let trimStart1 d (x:string) = x.TrimStart(d |> toCharArray)
let (@@) subPath path = 
    subPath 
    |> trimStart1 "\\"
    |> trimStart1 "/"
    |> fun x -> Path.Combine(path,subPath)
//Assembly.GetExecutingAssembly().Location.Dump()
module Reflection = 
    let cast<'t> (x:obj) = x :?> 't
    let getType (x:obj) = x.GetType()
    let getReferenceInfo() = 
        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.map(fun asm -> 
            asm.FullName
        )
        |> Dump
        |> ignore
    type Invokable = 
        | StaticMethod
        | InstanceMethod of obj
        
    let invokeMethod (t:Type) (name:string) inv = 
        match inv with
        | StaticMethod ->
            null
        | InstanceMethod x ->
            x
        |> fun x ->
            let m = t.GetMethod(name)
            fun args -> 
                m.Invoke(x,parameters=args)
    let invokeF t name inv =
        invokeMethod t name inv [| |]
    // for calling the same function passing different instances
    let createInvokable (t:Type) name =
        let m = t.GetMethod(name)
        fun x ->
            m.Invoke(x,[| |])
            
module Hack = 
    open Reflection
        
    let createGetFullNameFun t (x:obj) = 
        let f = Reflection.createInvokable t "get_FullName"
        f x |> cast<string>
    let createGetNameFun t (x:obj) = 
        let f = Reflection.createInvokable t "get_Name"
        f x |> cast<string>
        
    let getZipEntries actualT x =         
            //getReferenceInfo()
        let fMe = invokeMethod actualT "get_Entries" (InstanceMethod x) 
        let entries = fMe [| |] :?> IReadOnlyCollection<obj>
        let itemType = Seq.head entries |> getType
        let fFullName = itemType |> createGetFullNameFun
        let fName = itemType |> createGetNameFun
        entries 
        |> Seq.map(fun x ->  fName x, fFullName x,x)
            
    type ZipProxy(za:obj) =
        let za = za :?> IDisposable
        let t = za.GetType()
        let entryT, entries = 
            let entries = getZipEntries t za
            entries |> Seq.map(fun (_,_,x) -> x) |> Seq.head |> getType, entries
        let tryFind n =  entries |> Seq.tryFind (fun (_name,fn,x) -> fn = n)|> Option.map(fun (_,_,x) -> x)
        
        member __.TryExtract fullName (target:string) = 
            
            tryFind fullName
            |> Option.map(fun ze ->
                printfn "Extracting %s" fullName
                try
                    let lastWriteTime = invokeF entryT "get_LastWriteTime" (InstanceMethod ze) :?> DateTimeOffset
                    use stream = invokeMethod entryT "Open" (InstanceMethod ze) [| |] :?> Stream 
                    use destination = File.Open(target, FileMode.CreateNew, FileAccess.Write,FileShare.None)
                    stream.CopyTo destination
                    File.SetLastWriteTime(target,lastWriteTime.DateTime)
                    printfn "Extracted %s to %s" fullName target
                    destination.Dispose()
                    target
                with ex -> 
                    ze.Dump("failing to extract")
                    entryT.GetMethods()
                    |> Seq.map(fun m -> 
                        m.Name
                    )
                    |> fun x -> x.Dump(entryT.Name)
                    |> ignore
                    reraise()
            )
            
        interface IDisposable with
            member __.Dispose() = za.Dispose()
        
        
    // work around missing method exception
    let openRead (zipPath:string) = 
        let t = typeof<System.IO.Compression.ZipFile>
        let m = t.GetMethod("OpenRead")
        let x = m.Invoke(null,[| box zipPath|]) :?> IDisposable
        new ZipProxy(x)
    
let targetish = Environment.ExpandEnvironmentVariables("%droproot%")
let targets = [ targetish; Path.GetDirectoryName targetish @@ "Pm_VsBuild_Debug_Deploy"]
let exeName = "PracticeManagement.exe"
let subPath = @"drop\PM\" + exeName
let (|ExeDrop|_|) d = 
    let fp = Path.Combine(d,subPath)
    if File.Exists fp then
        Some fp
    else 
        try
            let extractedP = Path.Combine(d,"drop",exeName) 
            printfn "Checking for %s" extractedP
            if File.Exists extractedP then
                Some extractedP
            else 
                None
        with ex ->
            printfn "EXE Drop failing"
            reraise()
let (|ZipDrop|_|) d =
    let zipPath = Path.Combine(d,"drop")
    if Directory.Exists zipPath then
        Directory.GetFiles(zipPath,"*.zip") 
        |> Seq.tryHead
        |> function
            | Some zfp ->
                printfn "Found a zip file! %s" zfp
                use zp = Hack.openRead(zfp)
                let targetEntry = "b/PM/" + exeName
                let targetOut = Path.Combine(zipPath,exeName)
                zp.TryExtract targetEntry targetOut
            | None -> None
    else None
targets
|> Seq.collect Directory.EnumerateDirectories 
|> Seq.choose
    (function
    | ExeDrop fp ->
        Some fp
    | ZipDrop fp ->
        
//        failwith "uhhh not implemented"
        //zip.Entries.Dump()
        Some null
    | d ->
            printfn "nothing found to check at %s" d
            (Directory.GetFiles d).Dump("one of these?")
            None
)
|> Seq.truncate 3
|> Seq.sort
|> List.ofSeq
|> List.rev
|> List.truncate 20
|> List.map(fun fp ->
    try
        let fi = FileInfo(fp)
        let fvi = FileVersionInfo.GetVersionInfo fp
        fp,fvi.FileVersion,fi.CreationTime, fi.LastWriteTime
    with ex ->
        printfn "Failing on %s" fp
        reraise()
)
|> Dump
|> ignore